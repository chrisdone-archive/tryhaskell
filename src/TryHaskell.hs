{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Try Haskell!

module TryHaskell where

import           Paths_tryhaskell

import qualified Blaze as H
import           Blaze hiding (html,param,i)
import           Blaze.Bootstrap
import qualified Blaze.Elements as E
import           Control.Monad.Trans
import           Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (fromChunks)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (unpack)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Prelude hiding (div,head)
import           PureIO (Interrupt(..),Output(..),Input(..),IOException(..))
import           Snap.Core
import           Snap.Http.Server hiding (Config)
import           Snap.Util.FileServe
import           System.Exit
import           System.IO (stderr, hPutStrLn)
import           System.Process.Text.Lazy

data EvalResult
  = ErrorResult Text
  | SuccessResult (Text,Text,Text) [Text]
  | GetInputResult [Text]
  deriving (Show)

-- | Start a web server.
startServer :: IO ()
startServer =
  do checkMuEval
     httpServe server dispatch
  where server = setDefaults defaultConfig
        setDefaults =
          setPort 4001 .
          setVerbose False .
          setErrorLog ConfigNoLog .
          setAccessLog ConfigNoLog

-- | Ensure mueval is available and working
checkMuEval :: IO ()
checkMuEval =
  do result <- mueval False "()"
     case result of
       Left err -> die err
       _ -> return ()
  where
    die err = do hPutStrLn stderr ("ERROR: mueval " ++ msg err)
                 exitFailure
    msg err | T.null err = "failed to start"
            | otherwise  = "startup failure:\n" ++ T.unpack err

-- | Dispatch on the routes.
dispatch :: Snap ()
dispatch =
  route [("/static",serveDirectory "static")
        ,("/eval",eval)
        ,("/",home)]

-- | Evaluate the given expression.
eval :: Snap ()
eval =
  do mex <- getParam "exp"
     args <- getParam "args"
     case mex of
       Nothing -> error "exp expected"
       Just ex ->
         muevalToJson ex (getArgs (fmap (fromChunks . return) args)) >>= writeLBS . encode
  where getArgs args =
          fromMaybe [] (args >>= decode)

-- | Evaluate the given expression and return the result as a JSON value.
muevalToJson :: MonadIO m => ByteString -> [String] -> m Value
muevalToJson ex args =
  do result <- liftIO (muevalOrType (unpack (decodeUtf8 ex)) args)
     return
       (Aeson.object
          (case result of
             ErrorResult err ->
               [("error" .= err)]
             SuccessResult (expr,typ,value') stdouts ->
               [("success" .=
                 Aeson.object [("value"  .= value')
                              ,("expr"   .= expr)
                              ,("type"   .= typ)
                              ,("stdout" .= stdouts)])]
             GetInputResult stdouts ->
               [("stdout" .= stdouts)]))

-- | Try to evaluate the given expression. If there's a mueval error
-- (i.e. a compile error), then try just getting the type of the
-- expression.
muevalOrType :: String -> [String] -> IO EvalResult
muevalOrType e is =
  do result <- mueval False e
     case result of
       Left{} -> muevalIO e is
       Right r -> return (SuccessResult r [])

-- | Try to evaluate the expression as a (pure) IO action, if it type
-- checks and evaluates, then we're going to enter a potential
-- (referentially transparent) back-and-forth between the server and
-- the client.
muevalIO :: String -> [String] -> IO EvalResult
muevalIO e is =
  do result <- mueval False ("runTryHaskellIO " ++ show (Input is) ++ " (" ++ e ++ ")")
     case result of
       Left{} ->
         do result' <- mueval True e
            return
              (case result' of
                 Left err -> ErrorResult err
                 Right r -> SuccessResult r [])
       Right (_,_,read . T.unpack -> r) ->
         ioResult e r

-- | Extract an eval result from the IO reply.
ioResult :: String -> Either (Interrupt,Output) (String,Output) -> IO EvalResult
ioResult e r =
  case r of
    Left i ->
      case i of
        (InterruptException ex,_) ->
          return
            (ErrorResult
               (case ex of
                  UserError err -> T.pack err))
        (InterruptStdin,Output os) ->
          return (GetInputResult (map T.pack os))
    Right (value',Output os) -> do
      typ <- mueval True e
      return
        (case typ of
           Left err ->
             ErrorResult err
           Right (_,iotyp,_) ->
             SuccessResult (T.pack e,iotyp,T.pack value')
                           (map T.pack os))

-- | Evaluate the given expression and return either an error or an
-- (expr,type,value) triple.
mueval :: Bool -> String -> IO (Either Text (Text,Text,Text))
mueval typeOnly e =
  do importsfp <- getDataFileName "Imports.hs"
     (status,out,_) <- readProcessWithExitCode "mueval" (options importsfp) ""
     case status of
       ExitSuccess ->
         case drop 1 (T.lines out) of
           [typ,value'] -> return (Right (T.pack e,typ,value'))
           _ -> return (Left "Unable to get type and value of expression.")
       ExitFailure{} -> return (Left out)
  where options importsfp =
          ["-i","-t","1","--expression",e] ++
          ["--no-imports","-l",importsfp] ++
          ["--type-only" | typeOnly]

-- | The home page.
home :: Snap ()
home =
  writeLazyText
    (renderHtml (H.html (do head headContent
                            body bodyContent)))
  where headContent =
          do E.title "Try Haskell! An interactive tutorial in your browser"
             meta ! charset "utf-8"
             css "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
             css "/static/css/tryhaskell.css"
             css "//fonts.googleapis.com/css?family=Merriweather"
        css url =
          link ! rel "stylesheet"
               ! type_ "text/css"
               ! href url

-- | Content of the body.
bodyContent :: Html
bodyContent =
  do container (row (span12 bodyHeader))
     consoleArea
     bodyFooter
     scripts

-- | The header.
bodyHeader :: Html
bodyHeader =
  (div !. "haskell-icon-container")
    ((a ! href "/")
       (table (tr (do td ((p !. "haskell-icon") mempty)
                      (td !. "try-haskell") "Try Haskell"))))

-- | The white area in the middle.
consoleArea :: Html
consoleArea =
  (div !. "console")
    (container
       (row (do (span6 !# "console") mempty
                (span6 !# "guide") mempty)))

-- | The footer with links and such.
bodyFooter :: Html
bodyFooter =
  (div !. "footer")
    (container (row (span12 ((p !. "muted credit") links))))
  where links =
          do (a ! href "http://github.com/chrisdone/tryhaskell") "Try Haskell"
             " by "
             (a ! href "http://chrisdone.com/") "Chris Done"
             ", concept inspired by "
             (a ! href "http://tryruby.org/") "Try Ruby"
             ", Haskell evaluator powered by Gwern Branwen's "
             (a ! href "http://hackage.haskell.org/package/mueval") "Mueval"
             ",  and console by "
             (a ! href "http://github.com/chrisdone/jquery-console") "jquery-console"
             "."

-- | Scripts; jquery, console, tryhaskell, ga, the usual.
scripts :: Html
scripts =
  do (script ! src "http://code.jquery.com/jquery-2.0.3.min.js") mempty
     (script ! src "/static/js/jquery.console.js") mempty
     (script ! src "/static/js/tryhaskell.js") mempty
     (script ! src "/static/js/tryhaskell.pages.js") mempty
     script "var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");\
             \document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));"
     script "try {\
             \var pageTracker2 = _gat._getTracker(\"UA-7443395-14\");\
             \pageTracker2._setDomainName(\"none\");\
             \pageTracker2._setAllowLinker(true);\
             \pageTracker2._trackPageview(location.pathname + location.search + location.hash);\
             \window.ga_tracker = pageTracker2;\
             \} catch(err) {}"

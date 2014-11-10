{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Try Haskell!

module TryHaskell where

import           Paths_tryhaskell

import qualified Blaze as H
import           Blaze hiding (html,param,i,id)
import           Blaze.Bootstrap
import qualified Blaze.Elements as E
import           Control.Arrow ((***))
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson as Aeson
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (fromChunks)
import qualified Data.ByteString.Lazy as L (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Hashable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text (unpack)
import qualified Data.Text as S
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Time
import           Prelude hiding (div,head)
import           PureIO (Interrupt(..),Output(..),Input(..),IOException(..))
import           Safe
import           Snap.Core
import           Snap.Http.Server hiding (Config)
import           Snap.Util.FileServe
import           System.Environment (getEnvironment)
import           System.Exit
import           System.IO (stderr, hPutStrLn)
import           System.Locale
import           System.Process.Text.Lazy

data EvalResult
  = ErrorResult !Text
  | SuccessResult !(Text,Text,Text) ![Text] !(Map FilePath String)
  | GetInputResult ![Text] !(Map FilePath String)
  deriving (Show,Eq)

data Stats = Stats
  { statsUsers :: !(HashMap ByteString UTCTime) }

-- | Start a web server.
startServer :: IO ()
startServer =
  do checkMuEval
     env <- getEnvironment
     let port = maybe 4001 read $ lookup "PORT" env
     let config = setPort port
                . setAccessLog ConfigNoLog
                . setErrorLog ConfigNoLog
                . setVerbose False
                $ defaultConfig
     cache <- newMVar mempty
     stats <- newMVar (Stats mempty)
     void (forkIO (expireVisitors stats))
     void (forkIO (expireCache cache))
     httpServe config (dispatch cache stats)

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
dispatch :: MVar (HashMap (ByteString,ByteString) Value) -> MVar Stats -> Snap ()
dispatch cache stats =
  route [("/static",serveDirectory "static")
        ,("/eval",eval cache stats)
        ,("/users",users stats)
        ,("/",home stats)]

-- | Write out the list of current users.
users :: MVar Stats -> Snap ()
users statsv =
  do stats <- liftIO (readMVar statsv)
     writeLBS (encode (map (show . hash *** epoch)
                           (M.toList (statsUsers stats))))
  where epoch :: UTCTime -> Integer
        epoch = read . formatTime defaultTimeLocale "%s"

-- | Log the current user's visit to the stats table.
logVisit :: MVar Stats -> Snap ByteString
logVisit stats =
  do ipHeaderFilter
     addr <- fmap rqRemoteAddr getRequest
     now <- liftIO getCurrentTime
     let updateStats (Stats u) = Stats (M.insert addr now u)
     liftIO (modifyMVar_ stats (return . updateStats))
     return addr

-- | Reap visitors that have been inactive for one minute.
expireVisitors :: MVar Stats -> IO ()
expireVisitors stats =
  forever
    (do threadDelay (1000 * 1000 * 15)
        now <- getCurrentTime
        modifyMVar_ stats
                    (return .
                     Stats .
                     M.filter (not . (>60) . diffUTCTime now) .
                     statsUsers))

-- | Expire the cache once an hour.
expireCache :: MVar (HashMap (ByteString,ByteString) Value) -> IO ()
expireCache cache =
  forever
    (do threadDelay (1000 * 1000 * 60 * 60)
        modifyMVar_ cache (const (return mempty)))

-- | Evaluate the given expression.
eval :: MVar (HashMap (ByteString,ByteString) Value) -> MVar Stats -> Snap ()
eval cache stats =
  do mex <- getParam "exp"
     args <- getParam "args"
     case mex of
       Nothing -> error "exp expected"
       Just ex ->
         do let key = (ex,fromMaybe "" args)
            logit ex args
            liftIO (cachedEval key cache args ex) >>= jsonp
  where
        logit ex _ =
          do ip <- logVisit stats
             now <- liftIO getCurrentTime
             liftIO (appendFile "/tmp/tryhaskell-log"
                                 (show now ++  " " ++
                                  S.unpack (decodeUtf8 ip) ++
                                  "> " ++
                                  (S.unpack . decodeUtf8) ex ++
                                  "\n"))

-- | Read from the cache for the given expression (and context), or
-- otherwise generate the JSON.
cachedEval :: (Eq k, Hashable k)
           => k -> MVar (HashMap k Value) -> Maybe ByteString -> ByteString
           -> IO Value
cachedEval key cache args ex =
  do cacheMap <- readMVar cache
     case M.lookup key cacheMap of
       Just cached -> return cached
       Nothing ->
         do o <- case getArgs of
                   Nothing -> muevalToJson ex mempty mempty
                   Just (is,fs) -> muevalToJson ex is fs
            case o of
              (Object i)
                | Just _ <- M.lookup "error" i -> return ()
              _ ->
                modifyMVar_ cache (return . M.insert key o)
            return o
  where getArgs = fmap toLazy args >>= decode

-- | Output a JSON value, possibly wrapping it in a callback if one
-- was requested.
jsonp :: Value -> Snap ()
jsonp o =
  do mcallback <- getParam "callback"
     writeLBS (case mcallback of
                 Nothing -> encode o
                 Just c -> toLazy c <> "(" <> encode o <> ");")

-- | Evaluate the given expression and return the result as a JSON value.
muevalToJson :: MonadIO m => ByteString -> [String] -> Map FilePath String -> m Value
muevalToJson ex is fs =
  do result <- liftIO (muevalOrType (unpack (decodeUtf8 ex)) is fs)
     case result of
       ErrorResult "can't find file: Imports.hs\n" -> muevalToJson ex is fs
       ErrorResult e ->
         return
           (codify
              (ErrorResult
                 (if e == ""
                     then helpfulMsg
                     else e)))
       _ -> return (codify result)
  where helpfulMsg = "No result, evaluator might've been \
                     \killed due to heavy traffic. Retry?"
        codify result =
          Aeson.object
            (case result of
               ErrorResult err ->
                 [("error" .= err)]
               SuccessResult (expr,typ,value') stdouts files ->
                 [("success" .=
                   Aeson.object [("value"  .= value')
                                ,("expr"   .= expr)
                                ,("type"   .= typ)
                                ,("stdout" .= stdouts)
                                ,("files"  .= files)])]
               GetInputResult stdouts files ->
                 [("stdout" .= stdouts)
                 ,("files" .= files)])

-- | Strict bystring to lazy.
toLazy :: ByteString -> L.ByteString
toLazy = fromChunks . return

-- | Try to evaluate the given expression. If there's a mueval error
-- (i.e. a compile error), then try just getting the type of the
-- expression.
muevalOrType :: String -> [String] -> Map FilePath String -> IO EvalResult
muevalOrType e is fs =
  do typeResult <- mueval True e
     case typeResult of
       Left err ->
         return (ErrorResult err)
       Right (expr,typ,_) ->
         if T.isPrefixOf "IO " typ
            then muevalIO e is fs
            else
              do evalResult <- mueval False e
                 case evalResult of
                   Left err ->
                     if T.isPrefixOf "No instance for" err ||
                        T.isPrefixOf "Ambiguous " err
                        then return (SuccessResult (expr,typ,"") mempty fs)
                        else return (ErrorResult err)
                   Right (_,_,val) ->
                     return (SuccessResult (expr,typ,val) mempty fs)

-- | Try to evaluate the expression as a (pure) IO action, if it type
-- checks and evaluates, then we're going to enter a potential
-- (referentially transparent) back-and-forth between the server and
-- the client.
--
-- It handles stdin/stdout and files.
muevalIO :: String -> [String] -> Map FilePath String -> IO EvalResult
muevalIO e is fs =
  do result <- mueval False ("runTryHaskellIO " ++ show (convert (Input is fs)) ++ " (" ++ e ++ ")")
     case result of
       Left err ->
         return (ErrorResult err)
       Right (_,_,readMay . T.unpack -> Just r) ->
         ioResult e (bimap (second oconvert) (second oconvert) r)
       _ ->
         return (ErrorResult "Problem running IO.")
  where convert (Input os fs') = (os,Map.toList fs')
        oconvert (os,fs') = Output os (Map.fromList fs')

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
                  UserError err -> T.pack err
                  FileNotFound fp -> T.pack ("File not found: " <> fp)
                  DirectoryNotFound fp -> T.pack ("Directory not found: " <> fp)))
        (InterruptStdin,Output os fs) ->
          return (GetInputResult (map T.pack os) fs)
    Right (value',Output os fs) ->
      do typ <- mueval True e
         return
           (case typ of
              Left err ->
                ErrorResult err
              Right (_,iotyp,_) ->
                SuccessResult (T.pack e,iotyp,T.pack value')
                              (map T.pack os)
                              fs)

-- | Evaluate the given expression and return either an error or an
-- (expr,type,value) triple.
mueval :: Bool -> String -> IO (Either Text (Text,Text,Text))
mueval typeOnly e =
  do env <- getEnvironment
     importsfp <- getDataFileName "Imports.hs"
     let timeout = maybe "1" id $ lookup "MUEVAL_TIMEOUT" env
         options = ["-i","-t",timeout,"--expression",e] ++
                   ["--no-imports","-l",importsfp] ++
                   ["--type-only" | typeOnly]
     (status,out,err) <- readProcessWithExitCode "mueval" options ""
     case status of
       ExitSuccess ->
         case T.lines out of
           [e',typ,value'] | T.pack e == e' -> return (Right (T.pack e,typ,value'))
           _ -> do appendFile "/tmp/tryhaskell-log"
                               (e ++
                                " -> " ++
                                show out ++ " (bad output)" ++
                                "\n")
                   return (Left ("Unable to get type and value of expression: " <> T.pack e))
       ExitFailure{} ->
         case T.lines out of
           [e',_typ,value'] | T.pack e == e' -> return (Left value')
           [e',_typ]        | T.pack e == e' -> return (Left "Evaluation killed!")
           _ ->
             return (Left (out <> if out == "" then err <> " " <> T.pack (show status)  else ""))

-- | The home page.
home :: MVar Stats -> Snap ()
home stats =
  do logVisit stats
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
  do container
       (row (span12 (do bodyUsers
                        bodyHeader)))
     consoleArea
     bodyFooter
     scripts

-- | The active users display.
bodyUsers :: Html
bodyUsers =
  (div !. "active-users")
    (div "Active users")

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
             (a ! href "http://twitter.com/christopherdone") "@christopherdone"
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

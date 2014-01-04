{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Try Haskell!

module TryHaskell where

import qualified Blaze as H
import           Blaze hiding (html,param)
import           Blaze.Bootstrap
import qualified Blaze.Elements as E
import           Control.Monad.Trans
import           Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Text (unpack)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Prelude hiding (div,head)
import           Snap.Core
import           Snap.Http.Server hiding (Config)
import           Snap.Util.FileServe
import           System.Exit
import           System.Process.Text.Lazy

-- | Start a web server.
startServer :: IO ()
startServer =
  httpServe server dispatch
  where server =
          setPort 4001
                  (setVerbose False
                              (setErrorLog ConfigNoLog
                                           (setAccessLog ConfigNoLog
                                                         defaultConfig)))

-- | Dispatch on the routes.
dispatch :: Snap ()
dispatch = route [("/static",serveDirectory "static")
                 ,("/eval",eval)
                 ,("/",home)]

-- | Evaluate the given expression.
eval :: Snap ()
eval =
  do mex <- getParam "exp"
     case mex of
       Nothing -> error "exp expected"
       Just ex ->
         muevalToJson ex >>= writeLBS . encode

-- | Evaluate the given expression and return the result as a JSON value.
muevalToJson :: MonadIO m => ByteString -> m Value
muevalToJson ex =
  do result <- liftIO (mueval (unpack (decodeUtf8 ex)))
     return
       (Aeson.object
          (case result of
             Left err ->
               [("error" .= err)]
             Right (expr,typ,value') ->
               [("success" .=
                 Aeson.object [("value" .= value')
                              ,("expr" .= expr)
                              ,("type" .= typ)])]))

-- | Evaluate the given expression and return either an error or an
-- (expr,type,value) triple.
mueval :: String -> IO (Either Text (Text,Text,Text))
mueval e =
 do (status,out,_) <- readProcessWithExitCode "mueval" ["-i","-t","1","--expression",e] ""
    case status of
      ExitSuccess ->
        case drop 1 (T.lines out) of
          [typ,value'] -> return (Right (T.pack e,typ,value'))
          _ -> return (Left "Unable to get type and value of expression.")
      ExitFailure{} -> return (Left out)

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

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

startServer :: IO ()
startServer =
  httpServe server dispatch
  where server =
          setPort 4001
                  (setVerbose False
                              (setErrorLog ConfigNoLog
                                           (setAccessLog ConfigNoLog
                                                         defaultConfig)))

dispatch :: Snap ()
dispatch = route [("/static",serveDirectory "static")
                 ,("/eval",eval)
                 ,("/",home)]

eval :: Snap ()
eval =
  do mex <- getParam "exp"
     case mex of
       Nothing -> error "exp expected"
       Just ex ->
         do result <- liftIO (mueval (unpack (decodeUtf8 ex)))
            writeLBS
              (encode
                 (Aeson.object
                    (case result of
                       Left err ->
                         [("error" .= err)]
                       Right (expr,typ,value') ->
                         [("success" .=
                           Aeson.object [("value" .= value')
                                        ,("expr" .= expr)
                                        ,("type" .= typ)])])))

mueval :: String -> IO (Either Text (Text,Text,Text))
mueval e =
 do (status,out,_) <- readProcessWithExitCode "mueval" ["-i","-t","1","--expression",e] ""
    case status of
      ExitSuccess ->
        case drop 1 (T.lines out) of
          [typ,value'] -> return (Right (T.pack e,typ,value'))
          _ -> return (Left "Unable to get type and value of expression.")
      ExitFailure{} -> return (Left out)

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
        bodyContent =
          do container (do row (span12
                                  ((div !. "haskell-icon-container")
                                     ((a ! href "/")
                                        (table (tr (do td ((p !. "haskell-icon") mempty)
                                                       (td !. "try-haskell") "Try Haskell")))))))
             (div !. "console")
               (container
                  (row (do (span6 !# "console") mempty
                           (span6 !# "guide") mempty)))
             (div !. "footer")
               (container
                  (row (span12
                          ((p !. "muted credit")
                             (do (a ! href "http://github.com/chrisdone/tryhaskell") "Try Haskell"
                                 " by "
                                 (a ! href "http://chrisdone.com/") "Chris Done"
                                 ", concept inspired by "
                                 (a ! href "http://tryruby.org/") "Try Ruby"
                                 ", Haskell evaluator powered by Gwern Branwen's "
                                 (a ! href "http://hackage.haskell.org/package/mueval") "Mueval"
                                 ",  and console by "
                                 (a ! href "http://github.com/chrisdone/jquery-console") "jquery-console"
                                 ".")))))
             scripts
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
        css url =
          link ! rel "stylesheet"
               ! type_ "text/css"
               ! href url

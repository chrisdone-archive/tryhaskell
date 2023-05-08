module Main (main) where

import Control.Monad
import Network.Wai as Wai
import Control.Exception.Safe
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Control.Concurrent.Async
import Control.Concurrent
import qualified Data.ByteString.Char8 as S8
import System.IO
import System.Timeout
import Data.IORef
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Lucid hiding (for_)
import Data.Foldable
import Lucid.Base
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  run 3000 app

--------------------------------------------------------------------------------
-- Dispatcher

app :: Application
app request respond =
  case parseMethod (requestMethod request) of
    Right method ->
      case pathInfo request of
        [] ->
          case method of
            GET -> respond rootResponse
            _ -> respond invalidMethodResponse
        ["evaluator"] ->
          case method of
            GET ->
              case queryString request of
                [("code", code)] -> respond evaluatorResponse
                _ -> respond invalidArgumentResponse
            _ -> respond invalidMethodResponse
        _ -> respond pageNotFoundResponse
    _ -> respond invalidMethodResponse

--------------------------------------------------------------------------------
-- Generic HTTP responses

invalidMethodResponse :: Response
invalidMethodResponse = responseLBS status405 [("Content-Type","text/html; charset=utf-8")] $
  renderBS $
    doctypehtml_ do
      body_ do
        p_ "I don't support that method for that path."

invalidArgumentResponse :: Response
invalidArgumentResponse = responseLBS status400 [("Content-Type","text/html; charset=utf-8")] $
  renderBS $
    doctypehtml_ do
      body_ do
        p_ "Invalid arguments for this end-point."

pageNotFoundResponse :: Response
pageNotFoundResponse = responseLBS status404 [("Content-Type","text/html; charset=utf-8")] $
  renderBS $
    doctypehtml_ do
      body_ do
        p_ "No such path exists."

--------------------------------------------------------------------------------
-- Business logic responses

rootResponse :: Response
rootResponse = responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
  renderBS $
    doctypehtml_ do
      head_ do
        title_ "Try Haskell! An interactive tutorial in your browser"
        script_ [src_ "https://unpkg.com/htmx.org@1.9.2"
                ,integrity_ "sha384-L6OqL9pRWyyFU3+/bjdSri+iIphTN/bvYyM37tICVyOJkWZLpP2vGn6VUEXgzg6h", crossorigin_ "anonymous"]
                (mempty :: Text)
        script_ [src_ "https://unpkg.com/htmx.org/dist/ext/preload.js"
                ,crossorigin_ "anonymous"]
                (mempty :: Text)
      body_ [makeAttributes "hx-ext" "preload"] do
        evaluator_ Nothing

evaluatorResponse :: Response
evaluatorResponse = responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
  renderBS do
    evaluator_ (Just ())

--------------------------------------------------------------------------------
-- Htmx fragments

evaluator_ :: Maybe () -> Html ()
evaluator_ mresult =
  form_ [makeAttributes "hx-include" "*",
         makeAttributes "hx-get" "/evaluator"] do
    for_ mresult \_ -> p_ "Evaluator output..."
    textarea_ [name_ "code"] (pure ())
    button_ [makeAttributes "preload" "mousedown"] "Run"

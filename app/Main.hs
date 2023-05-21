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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- Duet dependencies
import qualified Control.Monad.Catch as Exceptions
import           Control.Monad.Logger
import           Control.Monad.Supply
import           Control.Monad.Writer
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))
import           Duet.Context
import           Duet.Errors
import           Duet.Infer
import           Duet.Parser
import           Duet.Printer
import           Duet.Renamer
import           Duet.Setup
import           Duet.Simple
import           Duet.Stepper
import           Duet.Types
import           System.IO

--------------------------------------------------------------------------------
-- Types

data Run = Run
  { runInputCode :: Text
  , runMainIs :: String
  , runConcise :: Bool
  , runNumbered :: Bool
  , runSteps :: Maybe Integer
  , runHideSteps :: Bool
  } deriving (Show)

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
                [("code", Just (T.decodeUtf8 -> code))] ->
                  evaluatorResponse code >>= respond
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
        intro_
        evaluator_ Nothing

evaluatorResponse :: Text -> IO Response
evaluatorResponse input = do
  output <- runProgram Run {
     runInputCode = input,
     runMainIs = "main",
     runConcise = True,
     runNumbered = False,
     runSteps = Just 100,
     runHideSteps = False
     }
  pure $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
    renderBS do
      reply_ (Just (input, output))

--------------------------------------------------------------------------------
-- Htmx fragments

intro_ :: Html ()
intro_ = do
  h1_ "Try Haskell"
  p_ do
    "This service gives you a small version Haskell, custom designed "
    "for teaching! You get the essence, without the distractions!"

-- | The evaluator form.
evaluator_ :: Maybe (Text, String) -> Html ()
evaluator_ minputOutput =
  form_ [makeAttributes "hx-include" "*",
         makeAttributes "hx-get" "/evaluator",
         makeAttributes "hx-target" "#reply"] do
    textarea_ [name_ "code"] (for_ minputOutput $ toHtml . fst)
    div_ [id_ "reply"] $ reply_ minputOutput
    button_ [makeAttributes "preload" "mousedown"] "Run"

-- | The reply from the evaluator.
reply_ :: Maybe (Text, String) -> Html ()
reply_ minputOutput = for_ minputOutput $ pre_ . toHtml . snd

--------------------------------------------------------------------------------
-- Code evaluation via Duet

runProgram :: Run -> IO String
runProgram run@Run {..} = do
  catchAny
    (runNoLoggingT
       (evalSupplyT
          (do decls <- liftIO (parseText "" runInputCode)
              (binds, ctx) <- createContext decls
              things <-
                execWriterT
                  (runStepperIO
                     run
                     runSteps
                     ctx
                     (fmap (fmap typeSignatureA) binds)
                     runMainIs)
              pure (concat things))
          [1 ..]))
    (pure . show)

-- | Run the substitution model on the code.
runStepperIO ::
     forall m. (MonadSupply Int m, MonadThrow m, MonadIO m, MonadWriter [String] m)
  => Run
  -> Maybe Integer
  -> Context Type Name Location
  -> [BindGroup Type Name Location]
  -> String
  -> m ()
runStepperIO Run {..} maxSteps ctx bindGroups' i = do
  e0 <- lookupNameByString i bindGroups'
  loop 1 "" e0
  where
    loop :: Integer -> String -> Expression Type Name Location -> m ()
    loop count lastString e = do
      e' <- expandSeq1 ctx bindGroups' e
      let string = printExpression (defaultPrint) e
      when
        (string /= lastString && not runHideSteps)
        (if cleanExpression e || not runConcise
           then
                  (tell [
                     ((if runNumbered
                         then "[" ++ show count ++ "]\n"
                         else "\n") ++
                      printExpression defaultPrint e) ])
           else pure ())
      if (fmap (const ()) e' /= fmap (const ()) e) &&
         case maxSteps of
           Just top -> count < top
           Nothing -> True
        then do
          newE <-
            renameExpression
              (contextSpecials ctx)
              (contextScope ctx)
              (contextDataTypes ctx)
              e'
          loop (count + 1) string newE
        else pure ()

-- | Filter out expressions with intermediate case, if and immediately-applied lambdas.
cleanExpression :: Expression Type i l -> Bool
cleanExpression =
  \case
    CaseExpression {} -> False
    IfExpression {} -> False
    e0
      | (LambdaExpression {}, args) <- fargs e0 -> null args
    ApplicationExpression _ f x -> cleanExpression f && cleanExpression x
    _ -> True

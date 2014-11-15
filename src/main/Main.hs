-- | Main entry point to tryhaskell.
--
-- Try Haskell!

module Main where

import Control.Concurrent
import Control.Exception
import TryHaskell

-- | Main entry point.
main :: IO ()
main =
  do ((stats,statsT),cache) <- setupServer
     finally (startServer cache stats)
             (killThread statsT)

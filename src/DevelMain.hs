-- | Main entry point to tryhaskell.
--
-- Try Haskell!

module DevelMain where

import Control.Concurrent
import Control.Exception
import Foreign.Store
import TryHaskell

-- | Main entry point.
main :: IO ()
main =
  do ((stats,statsT),cache) <- setupServer
     tid <- forkIO (finally (startServer cache stats)
                            (killThread statsT))
     putStrLn ("Writing store with " ++ show tid ++ " ...")
     writeStore (Store 0) tid

-- | Update the running server.
update :: IO ()
update =
  do m <- lookupStore 0
     case m of
       Nothing -> do putStrLn "Starting fresh ..."
                     main
       Just s ->
         do tid <- readStore s
            putStrLn ("Killing thread " ++ show tid)
            killThread tid
            main

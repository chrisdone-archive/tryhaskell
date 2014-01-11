module Imports where

import Prelude hiding (IO,putStr,putStrLn,getLine,readLn,print,readIO)
import PureIO as IO
import ShowFun
import Debug.SimpleReflect
import Data.Function
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Instances
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State
import Control.Monad.Writer
import Data.Array
import Data.Bits
import Data.Bool
import Data.Char
import Data.Complex
import Data.Dynamic
import Data.Either
import Data.Eq
import Data.Fixed
import Data.Graph
import Data.Int
import Data.Ix
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.Tree
import Data.Tuple
import Data.Typeable
import Data.Word
import System.Random
import Test.QuickCheck
import Text.PrettyPrint.HughesPJ
import Text.Printf

-- | Run the given command and then show the output. This constraint
-- is to aid communication between mueval and tryhaskell.
runTryHaskellIO :: (Read b,Show b) => Input -> IO b -> Either (Interrupt,Output) (String,Output)
runTryHaskellIO input m =
  case runIO input m of
    (Left i,out) -> Left (i,out)
    (Right r,out) -> Right (show r,out)

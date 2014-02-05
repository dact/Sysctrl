module Sysctrl.Init.State where

import Data.Sysctrl.Types.Internal.Automata
import Sysctrl.Util
import Sysctrl.Run.State (stateRun)

import qualified Data.Traversable as T (sequence)
import System.Posix.IO
import System.Posix.Types
import Data.Map (Map)
import qualified Data.Map as Map


stateInit :: State            -- State description
             -> Map String Fd -- Map with Fd for writes
             -> Fd            -- input Fd
             -> Fd            -- Control Fd
             -> Bool          -- Is final state?
             -> IO ()
stateInit m paths input ctrl _final = do
  let neighbors = map snd (delta m)
  let realPaths = mapFilter paths neighbors
  let uselessPaths = paths `Map.difference` realPaths
  T.sequence $ fmap closeFd uselessPaths
  let _delta = Map.fromList $ delta m
  stateRun realPaths _delta ctrl input _final

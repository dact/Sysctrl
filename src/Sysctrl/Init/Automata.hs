module Sysctrl.Init.Automata where

import Data.Sysctrl.Types.Internal.Automata
import Data.Sysctrl.Types.Util

import Sysctrl.Init.State (stateInit)
import Sysctrl.Util (doFdMap)
import System.Posix.Process (forkProcess)
import System.Posix.Types (Fd,ProcessID)
import System.Posix.IO (closeFd,stdInput)
import Data.List ((\\), find)
import qualified Data.Traversable as T (sequence)
import qualified Data.Map as Map
import Control.Applicative ((<$>))

automataInit :: Fd -> Fd -> Automata -> IO AutomatonProcess
automataInit ctrlW ctrlR auto=
  do
    -- All the pipes are initialized
    let _states         = map (state) (states auto)
    (readMap, writeMap) <- doFdMap _states
    let startNode       = start auto
    let startPipe       = case Map.lookup startNode writeMap of
	  Just a -> a
	  Nothing -> error "ERROR: no initial state found"
    let forkNodes _final st =
	  NodeP nm <$> prcss
	  where
	    prcss = forkProcess $ stateInit st writeMap input ctrlW _final
	    nm = state st
	    input = readMap Map.! nm
    let finals   = filter ((flip (elem) $ final auto).state) (states auto)
    let regulars = (states auto) \\ finals
    finalNodes   <- sequence $ map (forkNodes True) finals
    regularNodes <- sequence $ map (forkNodes False) regulars
    let allnodes = finalNodes ++ regularNodes
    let startPid = case find (( start auto ==).node) allnodes of
	  Just a -> nodePid a
	  Nothing -> error "No initial node"
    let closeWrite = Map.delete startNode writeMap
    let closeRead  = Map.delete startNode readMap
    T.sequence $ fmap (closeFd) closeWrite
    T.sequence $ fmap (closeFd) closeRead
    return $ AutoP startPid ctrlR startPipe allnodes

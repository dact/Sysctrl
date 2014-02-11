module Sysctrl.Util where

import System.Posix.Types
import System.Posix.IO
import Data.List
import System.Exit (exitSuccess)
import Control.Exception (catch)
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Sysctrl.Types.Automata as Ex
import qualified Data.Sysctrl.Types.Internal.Automata as In



errorHandler :: IOError -> IO a
errorHandler _ = exitSuccess >> error "ERROR"

transRead :: Fd -> IO String
transRead fd =
  do
    (readData, _ ) <- catch (fdRead fd 9) (errorHandler)
    let split = do
          i <- elemIndex '.' readData
          return $ splitAt i readData
    case split of
      Nothing -> error "Problem with the pipes"
      Just (n , '.':rest) ->
        do
          let size = ((read n) :: Int)
          if (size == (length rest))
            then return rest
            else do
            (msg,_) <- fdRead fd (fromIntegral $ size - (length rest))
            return $ rest ++ msg

transWrite :: Fd -> String -> IO ()
transWrite fd msg =
  do
    fdWrite fd $ (show $ length msg) ++ "." ++ msg
    return ()

_ExternalToInternal :: Ex.Automata -> In.Automata
_ExternalToInternal auto =
  In.Automata { In.name = Ex.automata auto
              , In.description = Ex.description auto
              , In.alpha = Ex.alpha auto
              , In.start = Ex.start auto
              , In.final = Ex.final auto
              , In.states = map _DeltaToState (Ex.delta auto)
              }

_DeltaToState :: Ex.Delta -> In.State
_DeltaToState _delta =
  In.State { In.state = Ex.node _delta ,
             In.delta = deltas $ Ex.trans _delta
           }
  where deltas = map (\tra -> ( Ex.symbol tra, Ex.next tra))

doFdMap :: [String] -> IO (Map String Fd, Map String Fd)
doFdMap [] = return (Map.empty, Map.empty)
doFdMap (x:xs) =
  do
    (r,w) <- createPipe
    let rm = Map.singleton x r
    let wm = Map.singleton x w
    (restR,restW) <- doFdMap xs
    let mapRead = rm `Map.union` restR
    let mapWrite = wm `Map.union` restW
    return (mapRead, mapWrite)

mapFilter :: (Ord k) => Map k a -> [k] -> Map k a
mapFilter _ [] = Map.empty
mapFilter m (x:xs) =
  case (Map.lookup x m) of
    Just a -> Map.singleton x a `Map.union` mapFilter m xs
    Nothing -> mapFilter m xs

correct :: In.Automata -> Bool
correct auto = startState && finalStates && deltas && alphabet
  where
    startState      = any ((==) (In.start auto).In.state) (In.states auto)
    finalStates     = all (`elem` statesNames) (In.final auto)
    statesOnDeltas  = (nub.concat) $ map (map snd.In.delta) (In.states auto)
    symbolsOnDeltas = (nub.concat) $ map (map fst.In.delta) (In.states auto)
    statesNames     = map (In.state) (In.states auto)
    deltas          = null $ statesOnDeltas \\ statesNames
    alphabet        = null $ symbolsOnDeltas \\ In.alpha auto

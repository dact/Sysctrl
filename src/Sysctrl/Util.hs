module Sysctrl.Util where

import System.Posix.Types
import System.Posix.IO
import Data.Maybe
import Data.List
import System.Exit (exitSuccess)
import qualified Control.Exception as E (catch)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Applicative ((<$>))

import qualified Data.Sysctrl.Types.Automata as Ex
import qualified Data.Sysctrl.Types.Internal.Automata as In
import qualified Data.Sysctrl.Types as T
import qualified Data.Sysctrl.Types.Util as Ut

errorHandler :: IOError -> IO a
errorHandler _ = exitSuccess >> error "ERROR"

transRead :: Fd -> IO String
transRead fd =
  do
    (readData, _ ) <- E.catch (fdRead fd 9) (errorHandler)
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


_AutoPToAutoI :: String -> Ut.AutomatonProcess -> T.AutoInfo
_AutoPToAutoI name auto = T.AutoInfo name (fromIntegral $ Ut.pid auto) nodeMap
  where
    nodeMap = map (_NodeToAutoInfo) (Ut.nodes auto)
    _NodeToAutoInfo _node = T.NodeInfo (Ut.node _node)
                            (fromIntegral $ Ut.nodePid _node)

_AutoMapToAutoList :: Map String Ut.AutoPar -> [T.AutoInfo]
_AutoMapToAutoList autoMap = map (_convert) autoList
  where
    autoList = Map.toList autoMap
    _convert (name, (Ut.AutoPar _ autoP)) = _AutoPToAutoI name autoP


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

swapStdin :: String -> IO ()
swapStdin file = do
  let openFlags = OpenFileFlags True False False False False
  nStdin <- openFd file ReadOnly Nothing openFlags
  dupTo nStdin stdInput
  return ()


correct_v :: In.Automata -> [T.AutoError]
correct_v auto = catMaybes $ map (mkError) errorList
  where
    (In.Automata name _ _ states start finals) = auto
    errorList = [c0,c1,c2,c3]
    mkError = (T.AutoError name <$>)
    statesNames = map (In.state) states
    -- #####################
    -- ## start state (c0)##
    -- #####################
    c0Q = any (((==) start).In.state) states
    c0ErrMsg ="The state named (" ++ start ++ ") is not in the set of states"
    c0 = if c0Q then Nothing else Just c0ErrMsg
    -- #####################
    -- ## final state (c1)##
    -- #####################
    c1ErrMsg n = "The final states " ++ (show n) ++
                 " are not in the set of states"
    c1 = case partition  (`elem` statesNames) finals of
      (_,[])     -> Nothing
      (_,missing) -> Just $ c1ErrMsg missing
    -- #####################
    -- ## deltas (c2)     ##
    -- #####################
    c2a  = (nub.concat) $ map (map snd.In.delta) states
    c2ErrMsg n = "Some deltas contains undefined states: " ++ n
    c2 = case c2a \\ statesNames of
      [] -> Nothing
      undef -> Just $ c2ErrMsg (show undef)
    -- #####################
    -- ## alphabet (c3)   ##
    -- #####################
    c3a = (nub.concat) $ map (map fst.In.delta) states
    c3ErrMsg n =  "Some deltas contains undefined symbols: [" ++ n ++"]"
    c3  = case c3a \\ In.alpha auto of
      [] -> Nothing
      undf -> Just $ c3ErrMsg undf

module Sysctrl.Run.State where

import Data.Sysctrl.Types
import Sysctrl.Util

import Data.Yaml
import System.Posix.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative
import Data.ByteString.Char8 (pack, unpack)


stateRun :: Map String Fd      -- Map with Fd for writes
	    -> Map Char String -- Delta map
	    -> Fd              -- Control pipe
	    -> Fd              -- Read pipe
	    -> Bool            -- Final?
	    -> IO ()
stateRun paths _delta ctrl input _final = do
  let _send t d= transWrite d ((unpack.encode) t)
  let ctrlError  (Msg eRec eRes) = _send (Result 1 eRec eRes) ctrl
  let ctrlAccept (Msg eRec eRes) = _send (Result 0 eRec eRes) ctrl
  _msg <- pack <$> transRead input
  let m@(Msg _rec _res) = case decode _msg of
	Just _m  -> _m
	Nothing -> Msg "" "_ERROR_"
  case _res of
    ""     -> if _final
	      then ctrlAccept m
	      else ctrlError m
    "_ERROR_" -> _send (Result 3 "INTERNAL ERROR" (unpack _msg)) ctrl
    (x:xs) ->
      case (Map.lookup x _delta >>= (flip Map.lookup) paths) of
	Just d -> _send nMsg d
	Nothing -> ctrlError m
      where
	nMsg = Msg (_rec ++ [x]) xs
  stateRun paths _delta ctrl input _final

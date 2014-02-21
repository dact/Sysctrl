module Sysctrl.Control where

import Data.Yaml
import Data.Sysctrl.Types
import Data.Sysctrl.Types.Util
import Sysctrl.Util

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Traversable as T (sequence)
import qualified Data.ByteString as B (ByteString, getLine)
import System.Posix.Types (Fd)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Data.ByteString.Char8 (pack, unpack)
import Control.Applicative ((<$>))


cmdRead :: B.ByteString -> Map String AutoPar -> IO ()
cmdRead _cmd autoData= do
  case (decodeEither _cmd :: Either String Cmd) of
    Left a -> putStrLn a
    (Right (Cmd "info" "")) -> cmdInfoAll autoData
    (Right (Cmd "info" a))  -> cmdInfoOne autoData a
    (Right (Cmd "send" a))  -> cmdSend autoData a
    (Right (Cmd "stop" _))  -> putStrLn "Closing.." >> exitSuccess
    (Right (Cmd a _))       -> putStrLn $ "Error: no command " ++ a

loop :: Map String AutoPar -> IO ()
loop autoData = do
  putStr "> " >> hFlush stdout
  line <- B.getLine
  cmdRead line autoData
  loop autoData


cmdInfoAll :: Map String AutoPar -> IO ()
cmdInfoAll autoData =
  putStrLn $ (unpack.encode) $ _data
  where
    _data = Response "info" $ Info $ _AutoMapToAutoList autoData

cmdInfoOne :: Map String AutoPar -> String -> IO ()
cmdInfoOne autoData a = case Map.lookup a autoData of
  Just (AutoPar _ auto) ->
    putStrLn $ (unpack.encode) $ Response "info" $ Info [_AutoPToAutoI a auto]
  Nothing   -> putStrLn $ "Error: no automata " ++ a

cmdSend :: Map String AutoPar -> String -> IO ()
cmdSend autoData m = do
  T.sequence $ Map.map (autoWrite) autoData
  waitOutput autoData
  where
    _msg = (unpack.encode) (Msg "" m)
    autoWrite auto = transWrite ((pipeW.system) auto) _msg

waitOutput :: Map String AutoPar -> IO ()
waitOutput _ = return ()

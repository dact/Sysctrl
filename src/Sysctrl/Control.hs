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


cmdRead :: Fd -> B.ByteString -> Map String AutoPar -> IO ()
cmdRead ctrl _cmd autoData= do
  case (decodeEither _cmd :: Either String Cmd) of
    Left a -> putStrLn a
    (Right (Cmd "info" ""))  -> cmdInfoAll autoData
    (Right (Cmd "info" a))   -> cmdInfoOne autoData a
    (Right (Cmd "send" a))  -> cmdSend autoData a ctrl
    (Right (Cmd "stop" _))  -> putStrLn "Closing.." >> exitSuccess
    (Right (Cmd a _))  -> putStrLn $ "Error: no command " ++ a

loop :: Fd -> Map String AutoPar -> IO ()
loop control autoData = do
  putStr "> " >> hFlush stdout
  line <- B.getLine
  cmdRead control line autoData
  loop control autoData


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

cmdSend :: Map String AutoPar -> String -> Fd -> IO ()
cmdSend autoData m control = do
  T.sequence $ Map.map (autoWrite) autoData
  waitOutput control (Map.size autoData)
  where
    _msg = (unpack.encode) (Msg "" m)
    autoWrite auto = transWrite ((pipe.system) auto) _msg

waitOutput :: Fd -> Int -> IO ()
waitOutput _ 0 = return ()
waitOutput control n = do
  _msg <- pack <$> transRead control
  let (Result cod _recog _rest) = case decode _msg of
        Just _m  -> _m
        Nothing -> Result 4 "_PARSE_ERROR_" (unpack _msg)
  case cod of
    0 -> putStrLn $ "Success whit String " ++ _recog
    1 -> putStrLn $ "Error recog: " ++ _recog ++ " rest: " ++ _rest
    3 -> putStrLn $ "Internal error on " ++ _rest
    _ -> putStrLn $ "unexpected error"
  waitOutput control (n-1)

module Sysctrl.Control where

import Data.Yaml
import Data.Sysctrl.Types
import Data.Sysctrl.Types.Util
import Sysctrl.Util
import qualified Data.Sysctrl.Types.Internal.Automata as I

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Traversable as T (sequence)
import qualified Data.ByteString as B (ByteString, getLine)
import qualified Data.Foldable as F
import System.Posix.Types (Fd)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Data.ByteString.Char8 (pack, unpack)
import Control.Applicative ((<$>))
import Control.Monad (when)


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
    (putStrLn.unpack.encode) $ Response "info" $ Info [_AutoPToAutoI a auto]
  Nothing   -> putStrLn $ "Error: no automata " ++ a

cmdSend :: Map String AutoPar -> String -> IO ()
cmdSend autoData m = do
  T.sequence $ Map.map (autoWrite) autoData
  waitOutput autoData
  where
    _msg = (unpack.encode) (Msg "" m)
    autoWrite auto = transWrite ((pipeW.system) auto) _msg

waitOutput :: Map String AutoPar -> IO ()
waitOutput autoMap = do
  resMap <- T.sequence $ fmap (readOutput) autoMap
  let (a,r,e) = filterResult resMap
  when (a /= Accept []) $ (putStrLn.unpack.encode) $ [Response "accept" a]
  when (r /= Reject []) $ (putStrLn.unpack.encode) $ [Response "reject" r]
  when (e /= Error []) $ (putStrLn.unpack.encode)  $ [Response "error" e]


readOutput :: AutoPar -> IO DataType
readOutput auto = do
  let control = (pipeR.system) auto
  _msg <- pack <$> transRead control
  let autoName = (I.name.info) auto
  let (Result cod _recog _rest) = case decode _msg of
        Just _m  -> _m
        Nothing -> Result 4 "_PARSE_ERROR_" (unpack _msg)
  return $ case cod of
    0 -> Accept [AutoAccept autoName _recog]
    1 -> Reject [AutoReject autoName (_recog ++ _rest) (length _recog)]
    3 -> Error [AutoError autoName _recog]
    _ -> Error [AutoError autoName _recog]

filterResult :: Map String DataType -> (DataType,DataType,DataType)
filterResult resMap = F.foldr (foldData) baseTuple resMap
  where
    foldData _data (Accept a, Reject r, Error e) =
      case _data of
        Accept _a -> (Accept $ a ++ _a, Reject r, Error e)
        Reject _r -> (Accept a, Reject $ r ++ _r, Error e)
        Error _e  -> (Accept a, Reject r, Error $ e ++ _e)
    baseTuple = (Accept [], Reject [], Error [])

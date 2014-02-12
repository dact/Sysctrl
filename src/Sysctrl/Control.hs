-- hola!!
module Sysctrl.Control where

import Data.Yaml
import Data.Sysctrl.Types
import Data.Sysctrl.Types.Util


import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Traversable as T (sequence)
import qualified Data.ByteString as B (ByteString, getLine)
import System.Posix.Types (Fd)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

cmdRead :: Fd -> B.ByteString -> Map String AutoPar -> IO ()
cmdRead ctrl _cmd autoData= do
  case (decodeEither _cmd :: Either String Cmd) of
    Left a -> putStrLn a
    (Right (Cmd "info" ""))  -> cmdInfoAll autoData
    (Right (Cmd "info" a))   -> cmdInfoOne autoData a
    (Right (Cmd "send" a))  -> cmdSend autoData a
    (Right (Cmd "stop" _))  -> putStrLn "Closing.." >> exitSuccess
    (Right (Cmd a _))  -> putStrLn $ "Error: no command " ++ a

loop :: Fd -> Map String AutoPar -> IO ()
loop control autoData = do
  putStr "> " >> hFlush stdout
  line <- B.getLine
  cmdRead control line autoData
  loop control autoData


cmdInfoAll :: Map String AutoPar -> IO ()
cmdInfoAll autoData = do
  T.sequence $ Map.mapWithKey autoPrint autoData
  return ()
  where
    autoPrint k a = putStrLn k >>  (print $ info a)

cmdInfoOne :: Map String AutoPar -> String -> IO ()
cmdInfoOne autoData a = case Map.lookup a autoData of
  Just auto -> print $ info auto
  Nothing   -> putStrLn $ "Error: no automata " ++ a

cmdSend :: Map String AutoPar -> String -> IO ()
cmdSend _ _ = return ()

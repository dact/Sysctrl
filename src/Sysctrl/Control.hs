module Sysctrl.Control where

import Data.Yaml
import Data.Sysctrl.Types
import Data.Sysctrl.Types.Util
import Data.Map (Map)

import qualified Data.ByteString as B (ByteString, getLine)
import System.Posix.Types (Fd)
import System.Exit (exitSuccess)

cmdRead :: B.ByteString -> IO ()
cmdRead _cmd = do
  case (decodeEither _cmd :: Either String Cmd) of
    Left a -> putStrLn a
    (Right (Cmd "info" ""))  -> putStrLn "info"
    (Right (Cmd "info" a))   -> putStrLn $ "info about " ++ a
    (Right (Cmd "send" a))  -> putStrLn $ "Sending message " ++ a
    (Right (Cmd "stop" _))  -> putStrLn "Closing.." >> exitSuccess
    (Right (Cmd a _))  -> putStrLn $ "Error: no command " ++ a

loop :: Fd -> Map String AutoPar -> IO ()
loop control autoData = do
  putStr "> "
  line <- B.getLine
  cmdRead line
  loop control autoData

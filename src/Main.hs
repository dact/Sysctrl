module Main where

import Data.ByteString (readFile, ByteString)
import Prelude hiding (init, readFile)
import System.Environment (getArgs)
import Sysctrl.Init (initRead, initRead_v)
import Data.List (sort)
import System.Console.GetOpt
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  (flags, args) <- getArgs >>= compilerOpts
  let msg = "Usage: sysctrl [-hvj] [-f File] <conf.yaml> "
  let helpmsg = putStrLn $ usageInfo msg options
  case (sort flags) of
    (Help:_) -> helpmsg >> exitSuccess
    (Version:_) -> putStrLn "Sysctrl v0.0.1" >> exitSuccess
    (File s:_) -> putStrLn "OMG"
    []         -> return ()
  file <- case args of
    (p:[]) -> readFile p
    []     -> helpmsg >> exitFailure
    _      -> helpmsg >> exitFailure
  initRead_v file
  return ()

data Flag = Help | Version | File String
	  deriving(Ord, Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help) "print Help"
  , Option ['v'] ["version"] (NoArg Version) "print version"
  , Option ['f'] ["file"] (ReqArg File "File") "use file as input commands"
  ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: sysctrl [-hvj] [-f File] choises..."

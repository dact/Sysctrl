module Main where

import Data.ByteString    (readFile)
import Prelude hiding     (init,readFile)
import System.Environment (getArgs)
import Data.List          (sort)
import System.Exit        (exitFailure,
			   exitSuccess)

import Sysctrl.Init       (initRead)
import Sysctrl.Util       (swapStdin)
import Sysctrl.Args       (helpmsg,
			   compilerOpts,
			   Flag (..))

main :: IO ()
main = do
  (flags, args) <- getArgs >>= compilerOpts
  case (sort flags) of
    (Help:_)     -> helpmsg >> exitSuccess
    (Version:_)  -> putStrLn "Sysctrl v0.0.1" >> exitSuccess
    ((File s):_) -> swapStdin s
    []           -> return ()
  case args of
    []     -> helpmsg    >> exitFailure
    (p:[]) -> readFile p >>= initRead
    _      -> helpmsg    >> exitFailure

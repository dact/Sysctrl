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
  tty <- case (sort flags) of
      []           -> return True
      (Help:_)     -> helpmsg >> exitSuccess >> return True
      (Version:_)  -> putStrLn "Sysctrl v0.0.1" >> exitSuccess >> return True
      ((File s):_) -> swapStdin s >> return False
  case args of
    []     -> helpmsg    >> exitFailure
    (p:[]) -> readFile p >>= \x -> initRead tty x
    _      -> helpmsg    >> exitFailure

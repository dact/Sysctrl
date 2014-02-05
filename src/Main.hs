{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.ByteString (readFile)
import Prelude hiding (init, readFile)
import System.Environment (getArgs)
import Sysctrl.Init (initRead, initRead_v)


main :: IO ()
main = do
  (path:_) <- getArgs
  file <- readFile path
  initRead_v file
  return ()

module Sysctrl.Args (helpmsg,
		     compilerOpts,
		     Flag (..)) where

import System.Console.GetOpt

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

helpmsg :: IO ()
helpmsg = putStrLn $ usageInfo msg options
	  where
	    msg = "Usage: sysctrl [-hvj] [-f File] <conf.yaml> "

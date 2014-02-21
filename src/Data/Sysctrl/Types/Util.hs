module Data.Sysctrl.Types.Util where

import System.Posix.Types (Fd,ProcessID)
import Data.Sysctrl.Types.Internal.Automata as I

data AutomatonProcess = AutoP { pid :: ProcessID,
				pipeR :: Fd,
				pipeW :: Fd,
				nodes :: [NodeProcess]
			      }deriving(Show)

data NodeProcess = NodeP { node :: String, nodePid :: ProcessID }
		 deriving(Show)

data AutoPar = AutoPar {info::I.Automata,system::AutomatonProcess}

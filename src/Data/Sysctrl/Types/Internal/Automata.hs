module Data.Sysctrl.Types.Internal.Automata (State(..),
                                             Automata(..))where

data State =
  State { state :: String,
          delta :: [(Char,String)]
        }deriving(Eq,Show)


data Automata =
  Automata { name :: String,
              description :: String,
              alpha :: String,
              states :: [State],
              start :: String,
              final :: [String]
            }deriving(Eq,Show)

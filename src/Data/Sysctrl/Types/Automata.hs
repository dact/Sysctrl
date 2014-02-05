{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sysctrl.Types.Automata where

import Data.Yaml
import GHC.Generics

data Trans = Trans { symbol :: Char
                   , next   :: String
                   }deriving (Show, Generic)

instance FromJSON Trans
instance ToJSON Trans

data Delta = Delta { node :: String
                   , trans :: [Trans]
                   } deriving (Show,Generic)

instance FromJSON Delta
instance ToJSON Delta

data Automata = Automata { automata :: String
                         , description :: String
                         , alpha :: [Char]
                         , states :: [String]
                         , start :: String
                         , final :: [String]
                         , delta :: [Delta]
                         } deriving (Show,Generic)

instance FromJSON Automata
instance ToJSON Automata

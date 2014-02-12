{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sysctrl.Types(Cmd(..),
			  Msg(..),
			  Result(..),
			  Response(..),
			  DataType(..),
			  NodeInfo(..),
			  AutoInfo(..),
			  AutoReject(..),
			  AutoAccept(..),
			  AutoError(..))
			  where

import GHC.Generics
import Data.Yaml
import Data.Vector (toList)
import Control.Applicative((<$>),(<*>),(<|>))
import Control.Monad(mzero)

--Represents the messages sent to the automatas
data Cmd = Cmd { cmd::String, msg::String }
	    deriving(Show,Eq,Generic)

instance ToJSON Cmd
instance FromJSON Cmd where
  parseJSON (Object v) = Cmd <$> v .: "cmd"
			     <*> v .:? "msg" .!= ""

--Represents the internal messages between states
data Msg = Msg { recog::String, rest::String }
	   deriving(Show,Eq,Generic)

instance ToJSON Msg
instance FromJSON Msg

--Represents the result message that will be sent to the control process
data Result = Result { codterm::Int, rRecog::String, rRest::String }
	      deriving(Show,Eq)

instance ToJSON Result where
  toJSON (Result code rc rs ) =
    object [ "codterm".= code,
	     "recog"  .= rc,
	     "rest"   .= rs
	   ]

instance FromJSON Result where
  parseJSON (Object v) = Result <$> v .: "codterm"
				<*> v .: "recog"
				<*> v .: "rest"
  parseJSON _ = mzero


data Response = Response { msgtype::String,
			   dataType::DataType
			 }deriving(Show,Eq)

instance ToJSON Response where
  toJSON (Response _msg _data) =
    object [ "msgtype" .= _msg,
	     case _data of
	       Info a   -> "info"   .= a
	       Reject a -> "reject" .= a
	       Accept a -> "accept" .= a
	       Error a  -> "error"  .= a
	   ]

instance FromJSON Response where
  parseJSON (Object v) = Response <$> v .: "msgtype"
				  <*> _data
    where
      _data = onEmpty (Info [])   <$> v .: "info"   <|>
	      onEmpty (Reject []) <$> v .: "reject" <|>
	      onEmpty (Accept []) <$> v .: "accept" <|>
	      onEmpty (Error [])  <$> v .: "error"
      onEmpty :: DataType -> DataType -> DataType
      onEmpty a (Info []) = a
      onEmpty a (Reject []) = a
      onEmpty a (Accept []) = a
      onEmpty a (Error [])  = a
      onEmpty _ a = a
  parseJSON _ = mzero

data DataType = Info [AutoInfo] |
		Reject [AutoReject] |
		Accept [AutoAccept] |
		Error [AutoError]
		deriving(Show,Eq)

instance ToJSON DataType where
  toJSON (Info v)   = array $ map (toJSON) v
  toJSON (Reject v) = array $ map (toJSON) v
  toJSON (Accept v) = array $ map (toJSON) v
  toJSON (Error v)  = array $ map (toJSON) v

instance FromJSON DataType where
  parseJSON (Array v) = Info   <$> _info <|>
			Reject <$> _reject <|>
			Accept <$> _accept <|>
			Error  <$> _error
    where
      _info   = mapM (parseJSON) (toList v)
      _reject = mapM (parseJSON) (toList v)
      _accept = mapM (parseJSON) (toList v)
      _error  = mapM (parseJSON) (toList v)



--Data type describing information about node process
data NodeInfo = NodeInfo { iNode::Char, iPid::Int }
		deriving(Show,Eq)

instance ToJSON NodeInfo where
  toJSON (NodeInfo _node _pid) =
    object ["node" .= _node,
	     "pid" .= _pid
	   ]

instance FromJSON NodeInfo where
  parseJSON (Object v) = NodeInfo <$> v .: "node"
				  <*> v .: "pid"
  parseJSON _ = mzero

--Data type describing information about automatas
data AutoInfo = AutoInfo { infoAutomata::String,
			   ppid::Int,
			   nodes::[NodeInfo]
			 }deriving(Show,Eq, Generic)

instance ToJSON AutoInfo where
  toJSON (AutoInfo _auto _pid _nodes) =
    object [ "automata" .= _auto,
	     "ppid".= _pid,
	     "nodes" .= _nodes ]

instance FromJSON AutoInfo where
  parseJSON (Object v) = AutoInfo <$> v .: "automata"
				  <*> v .: "ppid"
				  <*> v .:? "nodes" .!= []
  parseJSON _ = mzero

data AutoReject = AutoReject {rejectAutomata::String,
			      rMsg::String,
			      pos::Int
			     }deriving(Show,Eq)

instance ToJSON AutoReject where
  toJSON (AutoReject _auto _msg _pos) =
    object [ "automata" .= _auto,
	     "msg"      .= _msg,
	     "pos"      .= _pos
	   ]

instance FromJSON AutoReject where
  parseJSON (Object v) = AutoReject <$> v .: "automata"
				    <*> v .: "msg"
				    <*> v .: "pos"
  parseJSON _ = mzero

data AutoAccept = AutoAccept {acceptAutomata::String,
			      aMsg::String
			     }deriving(Show,Eq)

instance ToJSON AutoAccept where
  toJSON (AutoAccept _auto _msg) =
    object [ "automata" .= _auto,
	     "msg" .= _msg
	   ]

instance FromJSON AutoAccept where
  parseJSON (Object v) = AutoAccept <$> v .: "automata"
				    <*> v .: "msg"
  parseJSON _ = mzero

data AutoError = AutoError { eWhere::String,
			     eCause::String
			   }deriving(Show,Eq)

instance ToJSON AutoError where
  toJSON (AutoError _where _cause) =
    object [ "where" .= _where,
	     "cause" .= _cause
	   ]
instance FromJSON AutoError where
  parseJSON (Object v) = AutoError <$> v .: "where"
				   <*> v .: "cause"
  parseJSON _ = mzero

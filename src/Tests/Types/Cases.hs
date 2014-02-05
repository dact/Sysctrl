module Tests.Types.Cases where

import Data.Sysctrl.Types

newtype YamlTypes a = YamlTypes { getValue :: a }

--Test over Cmd Type

_Cmd_Test_1 :: YamlTypes Cmd
_Cmd_Test_1  = YamlTypes Cmd { cmd = "test1", msg="test1" }

_Cmd_Test_2 :: YamlTypes Cmd
_Cmd_Test_2  = YamlTypes Cmd { cmd = "test2", msg="" }

_Cmd_Test_3 :: YamlTypes Cmd
_Cmd_Test_3  = YamlTypes Cmd { cmd = "", msg="test3" }

--Test over Msg Type

_Msg_Test_1 :: YamlTypes Msg
_Msg_Test_1  = YamlTypes Msg { recog="test1", rest="test1" }

_Msg_Test_2 :: YamlTypes Msg
_Msg_Test_2  = YamlTypes Msg { recog="test2", rest="" }

_Msg_Test_3 :: YamlTypes Msg
_Msg_Test_3  = YamlTypes Msg { recog="", rest="test3" }

--Test over Result Type

_Result_Test_1 :: YamlTypes Result
_Result_Test_1 = YamlTypes Result{ codterm = 0,
                                   rRest = "test1",
                                   rRecog="test1"
                                 }

_Result_Test_2 :: YamlTypes Result
_Result_Test_2  = YamlTypes Result{ codterm = 0,
                                    rRest ="test2",
                                    rRecog=""
                                  }

_Result_Test_3 :: YamlTypes Result
_Result_Test_3  = YamlTypes Result{ codterm = 0,
                                    rRest = "",
                                    rRecog="test3"
                                  }

--Test over NodeInfo Type

_NodeInfo_Test_1 :: YamlTypes NodeInfo
_NodeInfo_Test_1 = YamlTypes NodeInfo { iNode='a',
                                        iPid=1000
                                      }
_NodeInfo_Test_2 :: YamlTypes NodeInfo
_NodeInfo_Test_2 = YamlTypes NodeInfo { iNode='b',
                                        iPid=2000
                                      }
_NodeInfo_Test_3 :: YamlTypes NodeInfo
_NodeInfo_Test_3 = YamlTypes NodeInfo { iNode='c',
                                        iPid=3000
                                      }
--Test over AutoInfo Type

_AutoInfo_Test_1 :: YamlTypes AutoInfo
_AutoInfo_Test_1  = YamlTypes AutoInfo { infoAutomata = "test1",
                                         ppid = 1000,
                                         nodes = []
                                       }
_AutoInfo_Test_2 :: YamlTypes AutoInfo
_AutoInfo_Test_2  = YamlTypes AutoInfo { infoAutomata = "test2",
                                         ppid = 2000,
                                         nodes = _nodes
                                       }
  where
    _nodes = map (getValue) [ _NodeInfo_Test_1 ]

_AutoInfo_Test_3 :: YamlTypes AutoInfo
_AutoInfo_Test_3  = YamlTypes AutoInfo { infoAutomata = "test3",
                                         ppid = 3000,
                                         nodes = _nodes
                                       }
  where
    _nodes = map (getValue) [ _NodeInfo_Test_1,
                              _NodeInfo_Test_2,
                              _NodeInfo_Test_3
                            ]

--Test over AutoReject Type

_AutoReject_Test_1 :: YamlTypes AutoReject
_AutoReject_Test_1  = YamlTypes AutoReject { rejectAutomata="test1",
                                             rMsg="test1",
                                             pos=1
                                           }

_AutoReject_Test_2 :: YamlTypes AutoReject
_AutoReject_Test_2  = YamlTypes AutoReject { rejectAutomata="test2",
                                             rMsg="test2",
                                             pos=2
                                           }

_AutoReject_Test_3 :: YamlTypes AutoReject
_AutoReject_Test_3  = YamlTypes AutoReject { rejectAutomata="test3",
                                             rMsg="test3",
                                             pos=3
                                           }

--Test over AutoAccept Type

_AutoAccept_Test_1 :: YamlTypes AutoAccept
_AutoAccept_Test_1  = YamlTypes AutoAccept {acceptAutomata="test1",
                                            aMsg="test1"
                                           }

_AutoAccept_Test_2 :: YamlTypes AutoAccept
_AutoAccept_Test_2  = YamlTypes AutoAccept {acceptAutomata="test2",
                                            aMsg="test2"
                                           }

_AutoAccept_Test_3 :: YamlTypes AutoAccept
_AutoAccept_Test_3  = YamlTypes AutoAccept {acceptAutomata="test3",
                                            aMsg="test3"
                                           }
--Test over AutoError Type

_AutoError_Test_1 :: YamlTypes AutoError
_AutoError_Test_1  = YamlTypes AutoError { eWhere = "test1",
                                           eCause = "test1"
                                         }

_AutoError_Test_2 :: YamlTypes AutoError
_AutoError_Test_2  = YamlTypes AutoError { eWhere = "test2",
                                           eCause = ""
                                         }

_AutoError_Test_3 :: YamlTypes AutoError
_AutoError_Test_3  = YamlTypes AutoError { eWhere = "",
                                           eCause = "test3"
                                         }

--Test over AutoError Type

_DataType_Info_Test_1 :: YamlTypes DataType
_DataType_Info_Test_1 = YamlTypes $ Info []

_DataType_Info_Test_2 :: YamlTypes DataType
_DataType_Info_Test_2 = YamlTypes $ Info _InfoList
  where
    _InfoList = map (getValue) [_AutoInfo_Test_1,
                                 _AutoInfo_Test_2,
                                 _AutoInfo_Test_3
                                ]

_DataType_Reject_Test_1 :: YamlTypes DataType
_DataType_Reject_Test_1 = YamlTypes $ Reject []

_DataType_Reject_Test_2 :: YamlTypes DataType
_DataType_Reject_Test_2 = YamlTypes $ Reject _RejectList
  where
    _RejectList = map (getValue) [_AutoReject_Test_1,
                                  _AutoReject_Test_2,
                                  _AutoReject_Test_3
                                 ]

_DataType_Accept_Test_1 :: YamlTypes DataType
_DataType_Accept_Test_1 = YamlTypes $ Accept []

_DataType_Accept_Test_2 :: YamlTypes DataType
_DataType_Accept_Test_2 = YamlTypes $ Accept _AcceptList
  where
    _AcceptList = map (getValue) [_AutoAccept_Test_1,
                                  _AutoAccept_Test_2,
                                  _AutoAccept_Test_3
                                 ]

_DataType_Error_Test_1 :: YamlTypes DataType
_DataType_Error_Test_1 = YamlTypes $ Error []

_DataType_Error_Test_2 :: YamlTypes DataType
_DataType_Error_Test_2 = YamlTypes $ Error _ErrorList
  where
    _ErrorList = map (getValue) [_AutoError_Test_1,
                                 _AutoError_Test_2,
                                 _AutoError_Test_3
                                ]

--Test over Response Type

_Response_Info_Test_1 :: YamlTypes Response
_Response_Info_Test_1  = YamlTypes Response{ msgtype = "info1",
                                        dataType = _data
                                      }
  where
    _data = getValue _DataType_Info_Test_1

_Response_Info_Test_2 :: YamlTypes Response
_Response_Info_Test_2  = YamlTypes Response{ msgtype = "info2",
                                        dataType = _data
                                      }
  where
    _data = getValue _DataType_Info_Test_2

_Response_Reject_Test_1 :: YamlTypes Response
_Response_Reject_Test_1  = YamlTypes Response{ msgtype = "reject1",
                                        dataType = _data
                                      }
  where
    _data = getValue _DataType_Reject_Test_1

_Response_Reject_Test_2 :: YamlTypes Response
_Response_Reject_Test_2  = YamlTypes Response{ msgtype = "reject2",
                                        dataType = _data
                                      }
  where
    _data = getValue _DataType_Reject_Test_2

_Response_Accept_Test_1 :: YamlTypes Response
_Response_Accept_Test_1  = YamlTypes Response{ msgtype = "accept1",
                                        dataType = _data
                                      }
  where
    _data = getValue _DataType_Accept_Test_1

_Response_Accept_Test_2 :: YamlTypes Response
_Response_Accept_Test_2  = YamlTypes Response{ msgtype = "accept2",
                                        dataType = _data
                                      }
  where
    _data = getValue _DataType_Accept_Test_2

_Response_Error_Test_1 :: YamlTypes Response
_Response_Error_Test_1  = YamlTypes Response{ msgtype = "accept1",
                                        dataType = _data
                                      }
  where
    _data = getValue _DataType_Error_Test_1

_Response_Error_Test_2 :: YamlTypes Response
_Response_Error_Test_2  = YamlTypes Response{ msgtype = "accept2",
                                        dataType = _data
                                      }
  where
    _data = getValue _DataType_Error_Test_2

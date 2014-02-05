module Tests.Types where

import Test.HUnit
import Data.Yaml
import Tests.Types.Cases

instance (ToJSON t, FromJSON t, Show t, Eq t)
         => Testable (YamlTypes t) where
  test _t = TestCase id_test
    where
      id_test = (Just t) @=? (decode.encode) t
      t = getValue _t


_TestList :: (Testable a) => (String,[a]) -> Test
_TestList (a,list) = TestLabel a $ TestList $ map (test) list

_Type_Tests :: Test
_Type_Tests =TestLabel "Types" $ TestList l
  where
    l = [ _TestList ("Cmd",
                     [_Cmd_Test_1,
                      _Cmd_Test_2,
                      _Cmd_Test_3]),
          _TestList ("Msg",
                     [_Msg_Test_1,
                      _Msg_Test_2,
                      _Msg_Test_3]),
          _TestList ("Result",
                     [_Result_Test_1,
                      _Result_Test_2,
                      _Result_Test_3]),
          _TestList ("NodeInfo",
                     [_NodeInfo_Test_1,
                      _NodeInfo_Test_2,
                      _NodeInfo_Test_3]),
          _TestList ("AutoInfo",
                     [_AutoInfo_Test_1,
                      _AutoInfo_Test_2,
                      _AutoInfo_Test_3]),
          _TestList ("AutoReject",
                     [_AutoReject_Test_1,
                      _AutoReject_Test_2,
                      _AutoReject_Test_3]),
          _TestList ("AutoAccept",
                     [_AutoAccept_Test_1,
                      _AutoAccept_Test_2,
                      _AutoAccept_Test_3]),
          _TestList ("AutoError",
                     [_AutoError_Test_1,
                      _AutoError_Test_2,
                      _AutoError_Test_3]),
          _TestList ("DataType_Info",
                     [--_DataType_Info_Test_1,
                      _DataType_Info_Test_2]),
          _TestList ("DataType_Reject",
                     [--_DataType_Reject_Test_1,
                      _DataType_Reject_Test_2]),
          _TestList ("DataType_Accept",
                     [--_DataType_Accept_Test_1,
                      _DataType_Accept_Test_2]),
          _TestList ("DataType_Error",
                     [--_DataType_Error_Test_1,
                      _DataType_Error_Test_2]),
          _TestList ("Response_Info",
                     [_Response_Info_Test_1,
                      _Response_Info_Test_2]),
          _TestList ("Response_Reject",
                     [_Response_Reject_Test_1,
                      _Response_Reject_Test_2]),
          _TestList ("Response_Accept",
                     [_Response_Accept_Test_1,
                      _Response_Accept_Test_2]),
          _TestList ("Response_Error",
                     [_Response_Error_Test_1,
                      _Response_Error_Test_2])
        ]

module Tests.Fun where

import Test.HUnit
import Tests.Fun.Cases

_Fun_Test :: Test
_Fun_Test = TestLabel "Functions" $ TestList l
  where
    l = [_correct_Test _Auto_In_1 True,
         _correct_Test _Auto_In_2 False,
         _correct_Test _Auto_In_3 False,
         _correct_Test _Auto_In_4 False,
-- It's true on paper, but in practice doesn't have sense
         _correct_Test _Auto_In_5 False
        ]

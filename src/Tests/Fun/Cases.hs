module Tests.Fun.Cases where

import Data.Sysctrl.Types.Internal.Automata as In
import Sysctrl.Util
import Test.HUnit

_Auto_In_1 :: In.Automata
_Auto_In_1 = In.Automata _name _desc _alph _states _start _final
  where
    _name = "In1"
    _desc = "Internal Automata Test 1"
    _alph =  "abcd"
    _states = [_a,_b,_c,_d]
    _a = In.State "a" [('b',"b"),('a',"a")]
    _b = In.State "b" [('c',"c"),('b',"b")]
    _c = In.State "c" [('d',"d"),('c',"c")]
    _d = In.State "d" [('a',"a"),('d',"d")]
    _start = "a"
    _final = ["c","d"]

_Auto_In_2 :: In.Automata
_Auto_In_2 = In.Automata _name _desc _alph _states _start _final
  where
    _name = "In2"
    _desc = "Internal Automata Test 2"
    _alph =  ""
    _states = [_a,_b,_c,_d]
    _a = In.State "a" [('b',"b"),('a',"a")]
    _b = In.State "b" [('c',"c"),('b',"b")]
    _c = In.State "c" [('d',"d"),('c',"c")]
    _d = In.State "d" [('a',"a"),('d',"d")]
    _start = "a"
    _final = ["c","d"]

_Auto_In_3 :: In.Automata
_Auto_In_3 = In.Automata _name _desc _alph _states _start _final
  where
    _name = "In3"
    _desc = "Internal Automata Test 3"
    _alph =  "abcd"
    _states = [_a,_b,_c]
    _a = In.State "a" [('b',"b"),('a',"a")]
    _b = In.State "b" [('c',"c"),('b',"b")]
    _c = In.State "c" [('d',"d"),('c',"c")]
    _start = "a"
    _final = ["c"]

_Auto_In_4 :: In.Automata
_Auto_In_4 = In.Automata _name _desc _alph _states _start _final
  where
    _name = "In4"
    _desc = "Internal Automata Test 4"
    _alph =  "abcd"
    _states = [_a,_b,_c]
    _a = In.State "a" [('b',"b"),('a',"a")]
    _b = In.State "b" [('c',"c"),('b',"b")]
    _c = In.State "c" [('a',"a"),('c',"c")]
    _start = "a"
    _final = ["c","d"]


_Auto_In_5 :: In.Automata
_Auto_In_5 = In.Automata [] [] [] [] [] []


_correct_Test :: In.Automata ->  Bool -> Test
_correct_Test auto isCorrect = TestCase $ isCorrect @=? correct auto

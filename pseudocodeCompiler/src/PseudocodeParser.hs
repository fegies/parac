{-# OPTIONS_GHC -w #-}
module PseudocodeParser (parsePSC) where
import Ast
import Tokens
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (LexToken)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 (Block)
	| HappyAbsSyn7 (Statement)
	| HappyAbsSyn8 ([String])
	| HappyAbsSyn12 (Expression)
	| HappyAbsSyn17 ([Expression])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (LexToken)
	-> HappyState (LexToken) (HappyStk HappyAbsSyn -> [(LexToken)] -> m HappyAbsSyn)
	-> [HappyState (LexToken) (HappyStk HappyAbsSyn -> [(LexToken)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(LexToken)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125 :: () => Int -> ({-HappyReduction (E) = -}
	   Int 
	-> (LexToken)
	-> HappyState (LexToken) (HappyStk HappyAbsSyn -> [(LexToken)] -> (E) HappyAbsSyn)
	-> [HappyState (LexToken) (HappyStk HappyAbsSyn -> [(LexToken)] -> (E) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(LexToken)] -> (E) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58 :: () => ({-HappyReduction (E) = -}
	   Int 
	-> (LexToken)
	-> HappyState (LexToken) (HappyStk HappyAbsSyn -> [(LexToken)] -> (E) HappyAbsSyn)
	-> [HappyState (LexToken) (HappyStk HappyAbsSyn -> [(LexToken)] -> (E) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(LexToken)] -> (E) HappyAbsSyn)

action_0 (19) = happyShift action_11
action_0 (23) = happyShift action_12
action_0 (26) = happyShift action_13
action_0 (28) = happyShift action_14
action_0 (31) = happyShift action_15
action_0 (32) = happyShift action_16
action_0 (33) = happyShift action_17
action_0 (34) = happyShift action_18
action_0 (38) = happyShift action_19
action_0 (60) = happyShift action_20
action_0 (61) = happyShift action_21
action_0 (62) = happyShift action_22
action_0 (63) = happyShift action_23
action_0 (4) = happyGoto action_24
action_0 (6) = happyGoto action_2
action_0 (7) = happyGoto action_3
action_0 (10) = happyGoto action_4
action_0 (11) = happyGoto action_5
action_0 (12) = happyGoto action_6
action_0 (13) = happyGoto action_7
action_0 (14) = happyGoto action_8
action_0 (15) = happyGoto action_9
action_0 (16) = happyGoto action_10
action_0 _ = happyFail

action_1 (19) = happyShift action_11
action_1 (23) = happyShift action_12
action_1 (26) = happyShift action_13
action_1 (28) = happyShift action_14
action_1 (31) = happyShift action_15
action_1 (32) = happyShift action_16
action_1 (33) = happyShift action_17
action_1 (34) = happyShift action_18
action_1 (38) = happyShift action_19
action_1 (60) = happyShift action_20
action_1 (61) = happyShift action_21
action_1 (62) = happyShift action_22
action_1 (63) = happyShift action_23
action_1 (6) = happyGoto action_2
action_1 (7) = happyGoto action_3
action_1 (10) = happyGoto action_4
action_1 (11) = happyGoto action_5
action_1 (12) = happyGoto action_6
action_1 (13) = happyGoto action_7
action_1 (14) = happyGoto action_8
action_1 (15) = happyGoto action_9
action_1 (16) = happyGoto action_10
action_1 _ = happyFail

action_2 (19) = happyShift action_11
action_2 (23) = happyShift action_12
action_2 (26) = happyShift action_13
action_2 (28) = happyShift action_14
action_2 (31) = happyShift action_15
action_2 (32) = happyShift action_16
action_2 (33) = happyShift action_17
action_2 (34) = happyShift action_18
action_2 (38) = happyShift action_19
action_2 (60) = happyShift action_20
action_2 (61) = happyShift action_21
action_2 (62) = happyShift action_22
action_2 (63) = happyShift action_23
action_2 (7) = happyGoto action_58
action_2 (10) = happyGoto action_4
action_2 (11) = happyGoto action_5
action_2 (12) = happyGoto action_6
action_2 (13) = happyGoto action_7
action_2 (14) = happyGoto action_8
action_2 (15) = happyGoto action_9
action_2 (16) = happyGoto action_10
action_2 _ = happyReduce_1

action_3 _ = happyReduce_5

action_4 _ = happyReduce_7

action_5 _ = happyReduce_8

action_6 (35) = happyShift action_38
action_6 (37) = happyShift action_39
action_6 (38) = happyShift action_40
action_6 (42) = happyShift action_41
action_6 (44) = happyShift action_42
action_6 (45) = happyShift action_43
action_6 (46) = happyShift action_44
action_6 (47) = happyShift action_45
action_6 (48) = happyShift action_46
action_6 (49) = happyShift action_47
action_6 (50) = happyShift action_48
action_6 (51) = happyShift action_49
action_6 (52) = happyShift action_50
action_6 (53) = happyShift action_51
action_6 (54) = happyShift action_52
action_6 (55) = happyShift action_53
action_6 (56) = happyShift action_54
action_6 (57) = happyShift action_55
action_6 (58) = happyShift action_56
action_6 (59) = happyShift action_57
action_6 _ = happyFail

action_7 _ = happyReduce_35

action_8 _ = happyReduce_32

action_9 _ = happyReduce_34

action_10 _ = happyReduce_33

action_11 (34) = happyShift action_18
action_11 (38) = happyShift action_19
action_11 (60) = happyShift action_20
action_11 (61) = happyShift action_21
action_11 (62) = happyShift action_22
action_11 (63) = happyShift action_23
action_11 (12) = happyGoto action_37
action_11 (13) = happyGoto action_7
action_11 (14) = happyGoto action_8
action_11 (15) = happyGoto action_9
action_11 (16) = happyGoto action_10
action_11 _ = happyFail

action_12 (34) = happyShift action_18
action_12 (38) = happyShift action_19
action_12 (60) = happyShift action_20
action_12 (61) = happyShift action_21
action_12 (62) = happyShift action_22
action_12 (63) = happyShift action_23
action_12 (12) = happyGoto action_36
action_12 (13) = happyGoto action_7
action_12 (14) = happyGoto action_8
action_12 (15) = happyGoto action_9
action_12 (16) = happyGoto action_10
action_12 _ = happyFail

action_13 (19) = happyShift action_11
action_13 (23) = happyShift action_12
action_13 (26) = happyShift action_13
action_13 (28) = happyShift action_14
action_13 (31) = happyShift action_15
action_13 (32) = happyShift action_16
action_13 (33) = happyShift action_17
action_13 (34) = happyShift action_18
action_13 (38) = happyShift action_19
action_13 (40) = happyShift action_35
action_13 (60) = happyShift action_20
action_13 (61) = happyShift action_21
action_13 (62) = happyShift action_22
action_13 (63) = happyShift action_23
action_13 (5) = happyGoto action_33
action_13 (7) = happyGoto action_34
action_13 (10) = happyGoto action_4
action_13 (11) = happyGoto action_5
action_13 (12) = happyGoto action_6
action_13 (13) = happyGoto action_7
action_13 (14) = happyGoto action_8
action_13 (15) = happyGoto action_9
action_13 (16) = happyGoto action_10
action_13 _ = happyReduce_2

action_14 (34) = happyShift action_18
action_14 (38) = happyShift action_19
action_14 (60) = happyShift action_20
action_14 (61) = happyShift action_21
action_14 (62) = happyShift action_22
action_14 (63) = happyShift action_23
action_14 (12) = happyGoto action_32
action_14 (13) = happyGoto action_7
action_14 (14) = happyGoto action_8
action_14 (15) = happyGoto action_9
action_14 (16) = happyGoto action_10
action_14 _ = happyFail

action_15 (62) = happyShift action_31
action_15 _ = happyFail

action_16 (34) = happyShift action_18
action_16 (35) = happyShift action_30
action_16 (38) = happyShift action_19
action_16 (60) = happyShift action_20
action_16 (61) = happyShift action_21
action_16 (62) = happyShift action_22
action_16 (63) = happyShift action_23
action_16 (12) = happyGoto action_29
action_16 (13) = happyGoto action_7
action_16 (14) = happyGoto action_8
action_16 (15) = happyGoto action_9
action_16 (16) = happyGoto action_10
action_16 _ = happyFail

action_17 (62) = happyShift action_28
action_17 _ = happyFail

action_18 (62) = happyShift action_27
action_18 _ = happyFail

action_19 (34) = happyShift action_18
action_19 (38) = happyShift action_19
action_19 (60) = happyShift action_20
action_19 (61) = happyShift action_21
action_19 (62) = happyShift action_22
action_19 (63) = happyShift action_23
action_19 (12) = happyGoto action_26
action_19 (13) = happyGoto action_7
action_19 (14) = happyGoto action_8
action_19 (15) = happyGoto action_9
action_19 (16) = happyGoto action_10
action_19 _ = happyFail

action_20 (34) = happyShift action_18
action_20 (38) = happyShift action_19
action_20 (60) = happyShift action_20
action_20 (61) = happyShift action_21
action_20 (62) = happyShift action_22
action_20 (63) = happyShift action_23
action_20 (12) = happyGoto action_25
action_20 (13) = happyGoto action_7
action_20 (14) = happyGoto action_8
action_20 (15) = happyGoto action_9
action_20 (16) = happyGoto action_10
action_20 _ = happyFail

action_21 _ = happyReduce_30

action_22 _ = happyReduce_37

action_23 _ = happyReduce_26

action_24 (64) = happyAccept
action_24 _ = happyFail

action_25 (37) = happyShift action_39
action_25 (38) = happyShift action_40
action_25 (42) = happyShift action_41
action_25 (56) = happyShift action_54
action_25 (57) = happyShift action_55
action_25 _ = happyReduce_54

action_26 (37) = happyShift action_39
action_26 (38) = happyShift action_40
action_26 (39) = happyShift action_87
action_26 (42) = happyShift action_41
action_26 (44) = happyShift action_42
action_26 (45) = happyShift action_43
action_26 (46) = happyShift action_44
action_26 (47) = happyShift action_45
action_26 (48) = happyShift action_46
action_26 (49) = happyShift action_47
action_26 (50) = happyShift action_48
action_26 (51) = happyShift action_49
action_26 (52) = happyShift action_50
action_26 (53) = happyShift action_51
action_26 (54) = happyShift action_52
action_26 (55) = happyShift action_53
action_26 (56) = happyShift action_54
action_26 (57) = happyShift action_55
action_26 (58) = happyShift action_56
action_26 (59) = happyShift action_57
action_26 _ = happyFail

action_27 _ = happyReduce_36

action_28 (40) = happyShift action_86
action_28 _ = happyFail

action_29 (35) = happyShift action_85
action_29 (37) = happyShift action_39
action_29 (38) = happyShift action_40
action_29 (42) = happyShift action_41
action_29 (44) = happyShift action_42
action_29 (45) = happyShift action_43
action_29 (46) = happyShift action_44
action_29 (47) = happyShift action_45
action_29 (48) = happyShift action_46
action_29 (49) = happyShift action_47
action_29 (50) = happyShift action_48
action_29 (51) = happyShift action_49
action_29 (52) = happyShift action_50
action_29 (53) = happyShift action_51
action_29 (54) = happyShift action_52
action_29 (55) = happyShift action_53
action_29 (56) = happyShift action_54
action_29 (57) = happyShift action_55
action_29 (58) = happyShift action_56
action_29 (59) = happyShift action_57
action_29 _ = happyFail

action_30 _ = happyReduce_9

action_31 (38) = happyShift action_84
action_31 _ = happyFail

action_32 (29) = happyShift action_82
action_32 (30) = happyShift action_83
action_32 (37) = happyShift action_39
action_32 (38) = happyShift action_40
action_32 (42) = happyShift action_41
action_32 (44) = happyShift action_42
action_32 (45) = happyShift action_43
action_32 (46) = happyShift action_44
action_32 (47) = happyShift action_45
action_32 (48) = happyShift action_46
action_32 (49) = happyShift action_47
action_32 (50) = happyShift action_48
action_32 (51) = happyShift action_49
action_32 (52) = happyShift action_50
action_32 (53) = happyShift action_51
action_32 (54) = happyShift action_52
action_32 (55) = happyShift action_53
action_32 (56) = happyShift action_54
action_32 (57) = happyShift action_55
action_32 (58) = happyShift action_56
action_32 (59) = happyShift action_57
action_32 _ = happyFail

action_33 (27) = happyShift action_81
action_33 _ = happyFail

action_34 _ = happyReduce_3

action_35 (19) = happyShift action_11
action_35 (23) = happyShift action_12
action_35 (26) = happyShift action_13
action_35 (28) = happyShift action_14
action_35 (31) = happyShift action_15
action_35 (32) = happyShift action_16
action_35 (33) = happyShift action_17
action_35 (34) = happyShift action_18
action_35 (38) = happyShift action_19
action_35 (60) = happyShift action_20
action_35 (61) = happyShift action_21
action_35 (62) = happyShift action_22
action_35 (63) = happyShift action_23
action_35 (6) = happyGoto action_80
action_35 (7) = happyGoto action_3
action_35 (10) = happyGoto action_4
action_35 (11) = happyGoto action_5
action_35 (12) = happyGoto action_6
action_35 (13) = happyGoto action_7
action_35 (14) = happyGoto action_8
action_35 (15) = happyGoto action_9
action_35 (16) = happyGoto action_10
action_35 _ = happyFail

action_36 (35) = happyShift action_79
action_36 (37) = happyShift action_39
action_36 (38) = happyShift action_40
action_36 (42) = happyShift action_41
action_36 (44) = happyShift action_42
action_36 (45) = happyShift action_43
action_36 (46) = happyShift action_44
action_36 (47) = happyShift action_45
action_36 (48) = happyShift action_46
action_36 (49) = happyShift action_47
action_36 (50) = happyShift action_48
action_36 (51) = happyShift action_49
action_36 (52) = happyShift action_50
action_36 (53) = happyShift action_51
action_36 (54) = happyShift action_52
action_36 (55) = happyShift action_53
action_36 (56) = happyShift action_54
action_36 (57) = happyShift action_55
action_36 (58) = happyShift action_56
action_36 (59) = happyShift action_57
action_36 _ = happyFail

action_37 (35) = happyShift action_78
action_37 (37) = happyShift action_39
action_37 (38) = happyShift action_40
action_37 (42) = happyShift action_41
action_37 (44) = happyShift action_42
action_37 (45) = happyShift action_43
action_37 (46) = happyShift action_44
action_37 (47) = happyShift action_45
action_37 (48) = happyShift action_46
action_37 (49) = happyShift action_47
action_37 (50) = happyShift action_48
action_37 (51) = happyShift action_49
action_37 (52) = happyShift action_50
action_37 (53) = happyShift action_51
action_37 (54) = happyShift action_52
action_37 (55) = happyShift action_53
action_37 (56) = happyShift action_54
action_37 (57) = happyShift action_55
action_37 (58) = happyShift action_56
action_37 (59) = happyShift action_57
action_37 _ = happyFail

action_38 _ = happyReduce_13

action_39 (62) = happyShift action_77
action_39 _ = happyFail

action_40 (34) = happyShift action_18
action_40 (38) = happyShift action_19
action_40 (60) = happyShift action_20
action_40 (61) = happyShift action_21
action_40 (62) = happyShift action_22
action_40 (63) = happyShift action_23
action_40 (12) = happyGoto action_74
action_40 (13) = happyGoto action_7
action_40 (14) = happyGoto action_8
action_40 (15) = happyGoto action_9
action_40 (16) = happyGoto action_10
action_40 (17) = happyGoto action_75
action_40 (18) = happyGoto action_76
action_40 _ = happyReduce_55

action_41 (34) = happyShift action_18
action_41 (38) = happyShift action_19
action_41 (60) = happyShift action_20
action_41 (61) = happyShift action_21
action_41 (62) = happyShift action_22
action_41 (63) = happyShift action_23
action_41 (12) = happyGoto action_73
action_41 (13) = happyGoto action_7
action_41 (14) = happyGoto action_8
action_41 (15) = happyGoto action_9
action_41 (16) = happyGoto action_10
action_41 _ = happyFail

action_42 (34) = happyShift action_18
action_42 (38) = happyShift action_19
action_42 (60) = happyShift action_20
action_42 (61) = happyShift action_21
action_42 (62) = happyShift action_22
action_42 (63) = happyShift action_23
action_42 (12) = happyGoto action_72
action_42 (13) = happyGoto action_7
action_42 (14) = happyGoto action_8
action_42 (15) = happyGoto action_9
action_42 (16) = happyGoto action_10
action_42 _ = happyFail

action_43 (34) = happyShift action_18
action_43 (38) = happyShift action_19
action_43 (60) = happyShift action_20
action_43 (61) = happyShift action_21
action_43 (62) = happyShift action_22
action_43 (63) = happyShift action_23
action_43 (12) = happyGoto action_71
action_43 (13) = happyGoto action_7
action_43 (14) = happyGoto action_8
action_43 (15) = happyGoto action_9
action_43 (16) = happyGoto action_10
action_43 _ = happyFail

action_44 (34) = happyShift action_18
action_44 (38) = happyShift action_19
action_44 (60) = happyShift action_20
action_44 (61) = happyShift action_21
action_44 (62) = happyShift action_22
action_44 (63) = happyShift action_23
action_44 (12) = happyGoto action_70
action_44 (13) = happyGoto action_7
action_44 (14) = happyGoto action_8
action_44 (15) = happyGoto action_9
action_44 (16) = happyGoto action_10
action_44 _ = happyFail

action_45 (34) = happyShift action_18
action_45 (38) = happyShift action_19
action_45 (60) = happyShift action_20
action_45 (61) = happyShift action_21
action_45 (62) = happyShift action_22
action_45 (63) = happyShift action_23
action_45 (12) = happyGoto action_69
action_45 (13) = happyGoto action_7
action_45 (14) = happyGoto action_8
action_45 (15) = happyGoto action_9
action_45 (16) = happyGoto action_10
action_45 _ = happyFail

action_46 (34) = happyShift action_18
action_46 (38) = happyShift action_19
action_46 (60) = happyShift action_20
action_46 (61) = happyShift action_21
action_46 (62) = happyShift action_22
action_46 (63) = happyShift action_23
action_46 (12) = happyGoto action_68
action_46 (13) = happyGoto action_7
action_46 (14) = happyGoto action_8
action_46 (15) = happyGoto action_9
action_46 (16) = happyGoto action_10
action_46 _ = happyFail

action_47 (34) = happyShift action_18
action_47 (38) = happyShift action_19
action_47 (60) = happyShift action_20
action_47 (61) = happyShift action_21
action_47 (62) = happyShift action_22
action_47 (63) = happyShift action_23
action_47 (12) = happyGoto action_67
action_47 (13) = happyGoto action_7
action_47 (14) = happyGoto action_8
action_47 (15) = happyGoto action_9
action_47 (16) = happyGoto action_10
action_47 _ = happyFail

action_48 (34) = happyShift action_18
action_48 (38) = happyShift action_19
action_48 (60) = happyShift action_20
action_48 (61) = happyShift action_21
action_48 (62) = happyShift action_22
action_48 (63) = happyShift action_23
action_48 (12) = happyGoto action_66
action_48 (13) = happyGoto action_7
action_48 (14) = happyGoto action_8
action_48 (15) = happyGoto action_9
action_48 (16) = happyGoto action_10
action_48 _ = happyFail

action_49 (34) = happyShift action_18
action_49 (38) = happyShift action_19
action_49 (60) = happyShift action_20
action_49 (61) = happyShift action_21
action_49 (62) = happyShift action_22
action_49 (63) = happyShift action_23
action_49 (12) = happyGoto action_65
action_49 (13) = happyGoto action_7
action_49 (14) = happyGoto action_8
action_49 (15) = happyGoto action_9
action_49 (16) = happyGoto action_10
action_49 _ = happyFail

action_50 (34) = happyShift action_18
action_50 (38) = happyShift action_19
action_50 (60) = happyShift action_20
action_50 (61) = happyShift action_21
action_50 (62) = happyShift action_22
action_50 (63) = happyShift action_23
action_50 (12) = happyGoto action_64
action_50 (13) = happyGoto action_7
action_50 (14) = happyGoto action_8
action_50 (15) = happyGoto action_9
action_50 (16) = happyGoto action_10
action_50 _ = happyFail

action_51 (34) = happyShift action_18
action_51 (38) = happyShift action_19
action_51 (60) = happyShift action_20
action_51 (61) = happyShift action_21
action_51 (62) = happyShift action_22
action_51 (63) = happyShift action_23
action_51 (12) = happyGoto action_63
action_51 (13) = happyGoto action_7
action_51 (14) = happyGoto action_8
action_51 (15) = happyGoto action_9
action_51 (16) = happyGoto action_10
action_51 _ = happyFail

action_52 (34) = happyShift action_18
action_52 (38) = happyShift action_19
action_52 (60) = happyShift action_20
action_52 (61) = happyShift action_21
action_52 (62) = happyShift action_22
action_52 (63) = happyShift action_23
action_52 (12) = happyGoto action_62
action_52 (13) = happyGoto action_7
action_52 (14) = happyGoto action_8
action_52 (15) = happyGoto action_9
action_52 (16) = happyGoto action_10
action_52 _ = happyFail

action_53 (34) = happyShift action_18
action_53 (38) = happyShift action_19
action_53 (60) = happyShift action_20
action_53 (61) = happyShift action_21
action_53 (62) = happyShift action_22
action_53 (63) = happyShift action_23
action_53 (12) = happyGoto action_61
action_53 (13) = happyGoto action_7
action_53 (14) = happyGoto action_8
action_53 (15) = happyGoto action_9
action_53 (16) = happyGoto action_10
action_53 _ = happyFail

action_54 _ = happyReduce_39

action_55 _ = happyReduce_40

action_56 (34) = happyShift action_18
action_56 (38) = happyShift action_19
action_56 (60) = happyShift action_20
action_56 (61) = happyShift action_21
action_56 (62) = happyShift action_22
action_56 (63) = happyShift action_23
action_56 (12) = happyGoto action_60
action_56 (13) = happyGoto action_7
action_56 (14) = happyGoto action_8
action_56 (15) = happyGoto action_9
action_56 (16) = happyGoto action_10
action_56 _ = happyFail

action_57 (34) = happyShift action_18
action_57 (38) = happyShift action_19
action_57 (60) = happyShift action_20
action_57 (61) = happyShift action_21
action_57 (62) = happyShift action_22
action_57 (63) = happyShift action_23
action_57 (12) = happyGoto action_59
action_57 (13) = happyGoto action_7
action_57 (14) = happyGoto action_8
action_57 (15) = happyGoto action_9
action_57 (16) = happyGoto action_10
action_57 _ = happyFail

action_58 _ = happyReduce_6

action_59 (37) = happyShift action_39
action_59 (38) = happyShift action_40
action_59 (42) = happyShift action_41
action_59 (45) = happyShift action_43
action_59 (46) = happyShift action_44
action_59 (47) = happyShift action_45
action_59 (48) = happyShift action_46
action_59 (49) = happyShift action_47
action_59 (50) = happyShift action_48
action_59 (51) = happyShift action_49
action_59 (52) = happyShift action_50
action_59 (53) = happyShift action_51
action_59 (54) = happyShift action_52
action_59 (55) = happyShift action_53
action_59 (56) = happyShift action_54
action_59 (57) = happyShift action_55
action_59 _ = happyReduce_53

action_60 (37) = happyShift action_39
action_60 (38) = happyShift action_40
action_60 (42) = happyShift action_41
action_60 (45) = happyShift action_43
action_60 (46) = happyShift action_44
action_60 (47) = happyShift action_45
action_60 (48) = happyShift action_46
action_60 (49) = happyShift action_47
action_60 (50) = happyShift action_48
action_60 (51) = happyShift action_49
action_60 (52) = happyShift action_50
action_60 (53) = happyShift action_51
action_60 (54) = happyShift action_52
action_60 (55) = happyShift action_53
action_60 (56) = happyShift action_54
action_60 (57) = happyShift action_55
action_60 (59) = happyShift action_57
action_60 _ = happyReduce_52

action_61 (37) = happyShift action_39
action_61 (38) = happyShift action_40
action_61 (42) = happyShift action_41
action_61 (56) = happyShift action_54
action_61 (57) = happyShift action_55
action_61 _ = happyReduce_45

action_62 (37) = happyShift action_39
action_62 (38) = happyShift action_40
action_62 (42) = happyShift action_41
action_62 (56) = happyShift action_54
action_62 (57) = happyShift action_55
action_62 _ = happyReduce_44

action_63 (37) = happyShift action_39
action_63 (38) = happyShift action_40
action_63 (42) = happyShift action_41
action_63 (56) = happyShift action_54
action_63 (57) = happyShift action_55
action_63 _ = happyReduce_43

action_64 (37) = happyShift action_39
action_64 (38) = happyShift action_40
action_64 (42) = happyShift action_41
action_64 (53) = happyShift action_51
action_64 (54) = happyShift action_52
action_64 (55) = happyShift action_53
action_64 (56) = happyShift action_54
action_64 (57) = happyShift action_55
action_64 _ = happyReduce_42

action_65 (37) = happyShift action_39
action_65 (38) = happyShift action_40
action_65 (42) = happyShift action_41
action_65 (53) = happyShift action_51
action_65 (54) = happyShift action_52
action_65 (55) = happyShift action_53
action_65 (56) = happyShift action_54
action_65 (57) = happyShift action_55
action_65 _ = happyReduce_41

action_66 (37) = happyShift action_39
action_66 (38) = happyShift action_40
action_66 (42) = happyShift action_41
action_66 (47) = happyShift action_45
action_66 (48) = happyShift action_46
action_66 (51) = happyShift action_49
action_66 (52) = happyShift action_50
action_66 (53) = happyShift action_51
action_66 (54) = happyShift action_52
action_66 (55) = happyShift action_53
action_66 (56) = happyShift action_54
action_66 (57) = happyShift action_55
action_66 _ = happyReduce_51

action_67 (37) = happyShift action_39
action_67 (38) = happyShift action_40
action_67 (42) = happyShift action_41
action_67 (47) = happyShift action_45
action_67 (48) = happyShift action_46
action_67 (51) = happyShift action_49
action_67 (52) = happyShift action_50
action_67 (53) = happyShift action_51
action_67 (54) = happyShift action_52
action_67 (55) = happyShift action_53
action_67 (56) = happyShift action_54
action_67 (57) = happyShift action_55
action_67 _ = happyReduce_50

action_68 (37) = happyShift action_39
action_68 (38) = happyShift action_40
action_68 (42) = happyShift action_41
action_68 (51) = happyShift action_49
action_68 (52) = happyShift action_50
action_68 (53) = happyShift action_51
action_68 (54) = happyShift action_52
action_68 (55) = happyShift action_53
action_68 (56) = happyShift action_54
action_68 (57) = happyShift action_55
action_68 _ = happyReduce_49

action_69 (37) = happyShift action_39
action_69 (38) = happyShift action_40
action_69 (42) = happyShift action_41
action_69 (51) = happyShift action_49
action_69 (52) = happyShift action_50
action_69 (53) = happyShift action_51
action_69 (54) = happyShift action_52
action_69 (55) = happyShift action_53
action_69 (56) = happyShift action_54
action_69 (57) = happyShift action_55
action_69 _ = happyReduce_48

action_70 (37) = happyShift action_39
action_70 (38) = happyShift action_40
action_70 (42) = happyShift action_41
action_70 (47) = happyShift action_45
action_70 (48) = happyShift action_46
action_70 (49) = happyShift action_47
action_70 (50) = happyShift action_48
action_70 (51) = happyShift action_49
action_70 (52) = happyShift action_50
action_70 (53) = happyShift action_51
action_70 (54) = happyShift action_52
action_70 (55) = happyShift action_53
action_70 (56) = happyShift action_54
action_70 (57) = happyShift action_55
action_70 _ = happyReduce_47

action_71 (37) = happyShift action_39
action_71 (38) = happyShift action_40
action_71 (42) = happyShift action_41
action_71 (47) = happyShift action_45
action_71 (48) = happyShift action_46
action_71 (49) = happyShift action_47
action_71 (50) = happyShift action_48
action_71 (51) = happyShift action_49
action_71 (52) = happyShift action_50
action_71 (53) = happyShift action_51
action_71 (54) = happyShift action_52
action_71 (55) = happyShift action_53
action_71 (56) = happyShift action_54
action_71 (57) = happyShift action_55
action_71 _ = happyReduce_46

action_72 (37) = happyShift action_39
action_72 (38) = happyShift action_40
action_72 (42) = happyShift action_41
action_72 (44) = happyShift action_42
action_72 (45) = happyShift action_43
action_72 (46) = happyShift action_44
action_72 (47) = happyShift action_45
action_72 (48) = happyShift action_46
action_72 (49) = happyShift action_47
action_72 (50) = happyShift action_48
action_72 (51) = happyShift action_49
action_72 (52) = happyShift action_50
action_72 (53) = happyShift action_51
action_72 (54) = happyShift action_52
action_72 (55) = happyShift action_53
action_72 (56) = happyShift action_54
action_72 (57) = happyShift action_55
action_72 (58) = happyShift action_56
action_72 (59) = happyShift action_57
action_72 _ = happyReduce_38

action_73 (37) = happyShift action_39
action_73 (38) = happyShift action_40
action_73 (42) = happyShift action_41
action_73 (43) = happyShift action_100
action_73 (44) = happyShift action_42
action_73 (45) = happyShift action_43
action_73 (46) = happyShift action_44
action_73 (47) = happyShift action_45
action_73 (48) = happyShift action_46
action_73 (49) = happyShift action_47
action_73 (50) = happyShift action_48
action_73 (51) = happyShift action_49
action_73 (52) = happyShift action_50
action_73 (53) = happyShift action_51
action_73 (54) = happyShift action_52
action_73 (55) = happyShift action_53
action_73 (56) = happyShift action_54
action_73 (57) = happyShift action_55
action_73 (58) = happyShift action_56
action_73 (59) = happyShift action_57
action_73 _ = happyFail

action_74 (37) = happyShift action_39
action_74 (38) = happyShift action_40
action_74 (42) = happyShift action_41
action_74 (44) = happyShift action_42
action_74 (45) = happyShift action_43
action_74 (46) = happyShift action_44
action_74 (47) = happyShift action_45
action_74 (48) = happyShift action_46
action_74 (49) = happyShift action_47
action_74 (50) = happyShift action_48
action_74 (51) = happyShift action_49
action_74 (52) = happyShift action_50
action_74 (53) = happyShift action_51
action_74 (54) = happyShift action_52
action_74 (55) = happyShift action_53
action_74 (56) = happyShift action_54
action_74 (57) = happyShift action_55
action_74 (58) = happyShift action_56
action_74 (59) = happyShift action_57
action_74 _ = happyReduce_57

action_75 (39) = happyShift action_99
action_75 _ = happyFail

action_76 (36) = happyShift action_98
action_76 _ = happyReduce_56

action_77 _ = happyReduce_29

action_78 (20) = happyShift action_97
action_78 _ = happyFail

action_79 (24) = happyShift action_96
action_79 _ = happyFail

action_80 (19) = happyShift action_11
action_80 (23) = happyShift action_12
action_80 (26) = happyShift action_13
action_80 (28) = happyShift action_14
action_80 (31) = happyShift action_15
action_80 (32) = happyShift action_16
action_80 (33) = happyShift action_17
action_80 (34) = happyShift action_18
action_80 (38) = happyShift action_19
action_80 (41) = happyShift action_95
action_80 (60) = happyShift action_20
action_80 (61) = happyShift action_21
action_80 (62) = happyShift action_22
action_80 (63) = happyShift action_23
action_80 (7) = happyGoto action_58
action_80 (10) = happyGoto action_4
action_80 (11) = happyGoto action_5
action_80 (12) = happyGoto action_6
action_80 (13) = happyGoto action_7
action_80 (14) = happyGoto action_8
action_80 (15) = happyGoto action_9
action_80 (16) = happyGoto action_10
action_80 _ = happyFail

action_81 (34) = happyShift action_18
action_81 (38) = happyShift action_19
action_81 (60) = happyShift action_20
action_81 (61) = happyShift action_21
action_81 (62) = happyShift action_22
action_81 (63) = happyShift action_23
action_81 (12) = happyGoto action_94
action_81 (13) = happyGoto action_7
action_81 (14) = happyGoto action_8
action_81 (15) = happyGoto action_9
action_81 (16) = happyGoto action_10
action_81 _ = happyFail

action_82 (34) = happyShift action_18
action_82 (38) = happyShift action_19
action_82 (60) = happyShift action_20
action_82 (61) = happyShift action_21
action_82 (62) = happyShift action_22
action_82 (63) = happyShift action_23
action_82 (12) = happyGoto action_93
action_82 (13) = happyGoto action_7
action_82 (14) = happyGoto action_8
action_82 (15) = happyGoto action_9
action_82 (16) = happyGoto action_10
action_82 _ = happyFail

action_83 (34) = happyShift action_18
action_83 (38) = happyShift action_19
action_83 (60) = happyShift action_20
action_83 (61) = happyShift action_21
action_83 (62) = happyShift action_22
action_83 (63) = happyShift action_23
action_83 (12) = happyGoto action_92
action_83 (13) = happyGoto action_7
action_83 (14) = happyGoto action_8
action_83 (15) = happyGoto action_9
action_83 (16) = happyGoto action_10
action_83 _ = happyFail

action_84 (62) = happyShift action_91
action_84 (9) = happyGoto action_90
action_84 _ = happyReduce_17

action_85 _ = happyReduce_10

action_86 (62) = happyShift action_89
action_86 (8) = happyGoto action_88
action_86 _ = happyReduce_14

action_87 _ = happyReduce_31

action_88 (41) = happyShift action_110
action_88 (62) = happyShift action_111
action_88 _ = happyFail

action_89 (35) = happyShift action_109
action_89 _ = happyFail

action_90 (36) = happyShift action_107
action_90 (39) = happyShift action_108
action_90 _ = happyFail

action_91 _ = happyReduce_18

action_92 (35) = happyShift action_106
action_92 (37) = happyShift action_39
action_92 (38) = happyShift action_40
action_92 (42) = happyShift action_41
action_92 (44) = happyShift action_42
action_92 (45) = happyShift action_43
action_92 (46) = happyShift action_44
action_92 (47) = happyShift action_45
action_92 (48) = happyShift action_46
action_92 (49) = happyShift action_47
action_92 (50) = happyShift action_48
action_92 (51) = happyShift action_49
action_92 (52) = happyShift action_50
action_92 (53) = happyShift action_51
action_92 (54) = happyShift action_52
action_92 (55) = happyShift action_53
action_92 (56) = happyShift action_54
action_92 (57) = happyShift action_55
action_92 (58) = happyShift action_56
action_92 (59) = happyShift action_57
action_92 _ = happyFail

action_93 (35) = happyShift action_105
action_93 (37) = happyShift action_39
action_93 (38) = happyShift action_40
action_93 (42) = happyShift action_41
action_93 (44) = happyShift action_42
action_93 (45) = happyShift action_43
action_93 (46) = happyShift action_44
action_93 (47) = happyShift action_45
action_93 (48) = happyShift action_46
action_93 (49) = happyShift action_47
action_93 (50) = happyShift action_48
action_93 (51) = happyShift action_49
action_93 (52) = happyShift action_50
action_93 (53) = happyShift action_51
action_93 (54) = happyShift action_52
action_93 (55) = happyShift action_53
action_93 (56) = happyShift action_54
action_93 (57) = happyShift action_55
action_93 (58) = happyShift action_56
action_93 (59) = happyShift action_57
action_93 _ = happyFail

action_94 (35) = happyShift action_104
action_94 (37) = happyShift action_39
action_94 (38) = happyShift action_40
action_94 (42) = happyShift action_41
action_94 (44) = happyShift action_42
action_94 (45) = happyShift action_43
action_94 (46) = happyShift action_44
action_94 (47) = happyShift action_45
action_94 (48) = happyShift action_46
action_94 (49) = happyShift action_47
action_94 (50) = happyShift action_48
action_94 (51) = happyShift action_49
action_94 (52) = happyShift action_50
action_94 (53) = happyShift action_51
action_94 (54) = happyShift action_52
action_94 (55) = happyShift action_53
action_94 (56) = happyShift action_54
action_94 (57) = happyShift action_55
action_94 (58) = happyShift action_56
action_94 (59) = happyShift action_57
action_94 _ = happyFail

action_95 _ = happyReduce_4

action_96 (19) = happyShift action_11
action_96 (23) = happyShift action_12
action_96 (26) = happyShift action_13
action_96 (28) = happyShift action_14
action_96 (31) = happyShift action_15
action_96 (32) = happyShift action_16
action_96 (33) = happyShift action_17
action_96 (34) = happyShift action_18
action_96 (38) = happyShift action_19
action_96 (40) = happyShift action_35
action_96 (60) = happyShift action_20
action_96 (61) = happyShift action_21
action_96 (62) = happyShift action_22
action_96 (63) = happyShift action_23
action_96 (5) = happyGoto action_103
action_96 (7) = happyGoto action_34
action_96 (10) = happyGoto action_4
action_96 (11) = happyGoto action_5
action_96 (12) = happyGoto action_6
action_96 (13) = happyGoto action_7
action_96 (14) = happyGoto action_8
action_96 (15) = happyGoto action_9
action_96 (16) = happyGoto action_10
action_96 _ = happyReduce_2

action_97 (19) = happyShift action_11
action_97 (23) = happyShift action_12
action_97 (26) = happyShift action_13
action_97 (28) = happyShift action_14
action_97 (31) = happyShift action_15
action_97 (32) = happyShift action_16
action_97 (33) = happyShift action_17
action_97 (34) = happyShift action_18
action_97 (38) = happyShift action_19
action_97 (40) = happyShift action_35
action_97 (60) = happyShift action_20
action_97 (61) = happyShift action_21
action_97 (62) = happyShift action_22
action_97 (63) = happyShift action_23
action_97 (5) = happyGoto action_102
action_97 (7) = happyGoto action_34
action_97 (10) = happyGoto action_4
action_97 (11) = happyGoto action_5
action_97 (12) = happyGoto action_6
action_97 (13) = happyGoto action_7
action_97 (14) = happyGoto action_8
action_97 (15) = happyGoto action_9
action_97 (16) = happyGoto action_10
action_97 _ = happyReduce_2

action_98 (34) = happyShift action_18
action_98 (38) = happyShift action_19
action_98 (60) = happyShift action_20
action_98 (61) = happyShift action_21
action_98 (62) = happyShift action_22
action_98 (63) = happyShift action_23
action_98 (12) = happyGoto action_101
action_98 (13) = happyGoto action_7
action_98 (14) = happyGoto action_8
action_98 (15) = happyGoto action_9
action_98 (16) = happyGoto action_10
action_98 _ = happyFail

action_99 _ = happyReduce_27

action_100 _ = happyReduce_28

action_101 (37) = happyShift action_39
action_101 (38) = happyShift action_40
action_101 (42) = happyShift action_41
action_101 (44) = happyShift action_42
action_101 (45) = happyShift action_43
action_101 (46) = happyShift action_44
action_101 (47) = happyShift action_45
action_101 (48) = happyShift action_46
action_101 (49) = happyShift action_47
action_101 (50) = happyShift action_48
action_101 (51) = happyShift action_49
action_101 (52) = happyShift action_50
action_101 (53) = happyShift action_51
action_101 (54) = happyShift action_52
action_101 (55) = happyShift action_53
action_101 (56) = happyShift action_54
action_101 (57) = happyShift action_55
action_101 (58) = happyShift action_56
action_101 (59) = happyShift action_57
action_101 _ = happyReduce_58

action_102 (21) = happyShift action_118
action_102 (22) = happyShift action_119
action_102 _ = happyFail

action_103 (25) = happyShift action_117
action_103 _ = happyFail

action_104 _ = happyReduce_23

action_105 (24) = happyShift action_116
action_105 _ = happyFail

action_106 (24) = happyShift action_115
action_106 _ = happyFail

action_107 (62) = happyShift action_114
action_107 _ = happyFail

action_108 (19) = happyShift action_11
action_108 (23) = happyShift action_12
action_108 (26) = happyShift action_13
action_108 (28) = happyShift action_14
action_108 (31) = happyShift action_15
action_108 (32) = happyShift action_16
action_108 (33) = happyShift action_17
action_108 (34) = happyShift action_18
action_108 (38) = happyShift action_19
action_108 (40) = happyShift action_35
action_108 (60) = happyShift action_20
action_108 (61) = happyShift action_21
action_108 (62) = happyShift action_22
action_108 (63) = happyShift action_23
action_108 (5) = happyGoto action_113
action_108 (7) = happyGoto action_34
action_108 (10) = happyGoto action_4
action_108 (11) = happyGoto action_5
action_108 (12) = happyGoto action_6
action_108 (13) = happyGoto action_7
action_108 (14) = happyGoto action_8
action_108 (15) = happyGoto action_9
action_108 (16) = happyGoto action_10
action_108 _ = happyReduce_2

action_109 _ = happyReduce_15

action_110 _ = happyReduce_12

action_111 (35) = happyShift action_112
action_111 _ = happyFail

action_112 _ = happyReduce_16

action_113 _ = happyReduce_11

action_114 _ = happyReduce_19

action_115 (19) = happyShift action_11
action_115 (23) = happyShift action_12
action_115 (26) = happyShift action_13
action_115 (28) = happyShift action_14
action_115 (31) = happyShift action_15
action_115 (32) = happyShift action_16
action_115 (33) = happyShift action_17
action_115 (34) = happyShift action_18
action_115 (38) = happyShift action_19
action_115 (40) = happyShift action_35
action_115 (60) = happyShift action_20
action_115 (61) = happyShift action_21
action_115 (62) = happyShift action_22
action_115 (63) = happyShift action_23
action_115 (5) = happyGoto action_122
action_115 (7) = happyGoto action_34
action_115 (10) = happyGoto action_4
action_115 (11) = happyGoto action_5
action_115 (12) = happyGoto action_6
action_115 (13) = happyGoto action_7
action_115 (14) = happyGoto action_8
action_115 (15) = happyGoto action_9
action_115 (16) = happyGoto action_10
action_115 _ = happyReduce_2

action_116 (19) = happyShift action_11
action_116 (23) = happyShift action_12
action_116 (26) = happyShift action_13
action_116 (28) = happyShift action_14
action_116 (31) = happyShift action_15
action_116 (32) = happyShift action_16
action_116 (33) = happyShift action_17
action_116 (34) = happyShift action_18
action_116 (38) = happyShift action_19
action_116 (40) = happyShift action_35
action_116 (60) = happyShift action_20
action_116 (61) = happyShift action_21
action_116 (62) = happyShift action_22
action_116 (63) = happyShift action_23
action_116 (5) = happyGoto action_121
action_116 (7) = happyGoto action_34
action_116 (10) = happyGoto action_4
action_116 (11) = happyGoto action_5
action_116 (12) = happyGoto action_6
action_116 (13) = happyGoto action_7
action_116 (14) = happyGoto action_8
action_116 (15) = happyGoto action_9
action_116 (16) = happyGoto action_10
action_116 _ = happyReduce_2

action_117 _ = happyReduce_22

action_118 (19) = happyShift action_11
action_118 (23) = happyShift action_12
action_118 (26) = happyShift action_13
action_118 (28) = happyShift action_14
action_118 (31) = happyShift action_15
action_118 (32) = happyShift action_16
action_118 (33) = happyShift action_17
action_118 (34) = happyShift action_18
action_118 (38) = happyShift action_19
action_118 (40) = happyShift action_35
action_118 (60) = happyShift action_20
action_118 (61) = happyShift action_21
action_118 (62) = happyShift action_22
action_118 (63) = happyShift action_23
action_118 (5) = happyGoto action_120
action_118 (7) = happyGoto action_34
action_118 (10) = happyGoto action_4
action_118 (11) = happyGoto action_5
action_118 (12) = happyGoto action_6
action_118 (13) = happyGoto action_7
action_118 (14) = happyGoto action_8
action_118 (15) = happyGoto action_9
action_118 (16) = happyGoto action_10
action_118 _ = happyReduce_2

action_119 _ = happyReduce_21

action_120 (22) = happyShift action_125
action_120 _ = happyFail

action_121 (25) = happyShift action_124
action_121 _ = happyFail

action_122 (25) = happyShift action_123
action_122 _ = happyFail

action_123 _ = happyReduce_25

action_124 _ = happyReduce_24

action_125 _ = happyReduce_20

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1:[]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1:[]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 ++ happy_var_2:[]
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  7 happyReduction_9
happyReduction_9 _
	_
	 =  HappyAbsSyn7
		 (StatementReturn EmptyExpression
	)

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (StatementReturn happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 6 7 happyReduction_11
happyReduction_11 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (PToken pos (TokenWord happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (StatementFunctionDeclaration happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 5 7 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (PToken pos (TokenWord happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (StatementClassDeclaration happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_2  7 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn7
		 (StatementExpression happy_var_1
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_0  8 happyReduction_14
happyReduction_14  =  HappyAbsSyn8
		 ([]
	)

happyReduce_15 = happySpecReduce_2  8 happyReduction_15
happyReduction_15 _
	(HappyTerminal (PToken pos (TokenWord happy_var_1)))
	 =  HappyAbsSyn8
		 (happy_var_1:[]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  8 happyReduction_16
happyReduction_16 _
	(HappyTerminal (PToken pos (TokenWord happy_var_2)))
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ happy_var_2:[]
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  9 happyReduction_17
happyReduction_17  =  HappyAbsSyn8
		 ([]
	)

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 (HappyTerminal (PToken pos (TokenWord happy_var_1)))
	 =  HappyAbsSyn8
		 (happy_var_1:[]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 (HappyTerminal (PToken pos (TokenWord happy_var_3)))
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ happy_var_3:[]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 8 10 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (StatementIf happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 6 10 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (StatementIf happy_var_2 happy_var_5 []
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 6 11 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (StatementWhile happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 5 11 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (StatementRepeat happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 8 11 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (StatementForTo happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 8 11 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (StatementForDownto happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_1  12 happyReduction_26
happyReduction_26 (HappyTerminal (PToken pos (TokenStringLit happy_var_1))
	 =  HappyAbsSyn12
		 (ExpressionConstant (ConstantString happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 12 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (ExpressionFunctionCall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 4 12 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (ExpressionArrayAccess happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_3  12 happyReduction_29
happyReduction_29 (HappyTerminal (PToken pos (TokenWord happy_var_3)))
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionObjectMembAccess happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  12 happyReduction_30
happyReduction_30 (HappyTerminal (PToken pos (TokenInt happy_var_1)))
	 =  HappyAbsSyn12
		 (ExpressionConstant (ConstantInt happy_var_1)
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  12 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  12 happyReduction_32
happyReduction_32 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  12 happyReduction_33
happyReduction_33 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  12 happyReduction_34
happyReduction_34 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  12 happyReduction_35
happyReduction_35 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  12 happyReduction_36
happyReduction_36 (HappyTerminal (PToken pos (TokenWord happy_var_2)))
	_
	 =  HappyAbsSyn12
		 (ExpressionObjectNew happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  12 happyReduction_37
happyReduction_37 (HappyTerminal (PToken pos (TokenWord happy_var_1)))
	 =  HappyAbsSyn12
		 (ExpressionVar happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  13 happyReduction_38
happyReduction_38 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionAssign happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  14 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionArithInc happy_var_1
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  14 happyReduction_40
happyReduction_40 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionArithDec happy_var_1
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  14 happyReduction_41
happyReduction_41 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionArithPlus happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  14 happyReduction_42
happyReduction_42 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionArithMinus happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  14 happyReduction_43
happyReduction_43 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionArithMul happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  14 happyReduction_44
happyReduction_44 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionArithDiv happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  14 happyReduction_45
happyReduction_45 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionArithMod happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  15 happyReduction_46
happyReduction_46 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionCompareEq happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  15 happyReduction_47
happyReduction_47 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionCompareNeq happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  15 happyReduction_48
happyReduction_48 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionCompareLt  happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  15 happyReduction_49
happyReduction_49 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionCompareLeq happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  15 happyReduction_50
happyReduction_50 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionCompareGt happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  15 happyReduction_51
happyReduction_51 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionCompareGeq happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  16 happyReduction_52
happyReduction_52 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionLogicAnd happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  16 happyReduction_53
happyReduction_53 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (ExpressionLogicOr happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  16 happyReduction_54
happyReduction_54 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (ExpressionLogicNot happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_0  17 happyReduction_55
happyReduction_55  =  HappyAbsSyn17
		 ([]
	)

happyReduce_56 = happySpecReduce_1  17 happyReduction_56
happyReduction_56 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  18 happyReduction_57
happyReduction_57 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 : []
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  18 happyReduction_58
happyReduction_58 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_3 : happy_var_1
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 64 64 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PToken pos TokenIf -> cont 19;
	PToken pos TokenThen -> cont 20;
	PToken pos TokenElse -> cont 21;
	PToken pos TokenFi -> cont 22;
	PToken pos TokenWhile -> cont 23;
	PToken pos TokenDo -> cont 24;
	PToken pos TokenOd -> cont 25;
	PToken pos TokenRepeat -> cont 26;
	PToken pos TokenUntil -> cont 27;
	PToken pos TokenFor -> cont 28;
	PToken pos TokenTo -> cont 29;
	PToken pos TokenDownto -> cont 30;
	PToken pos TokenFunction -> cont 31;
	PToken pos TokenReturn -> cont 32;
	PToken pos TokenClass -> cont 33;
	PToken pos TokenNew -> cont 34;
	PToken pos TokenSemicolon -> cont 35;
	PToken pos TokenComma -> cont 36;
	PToken pos TokenDot -> cont 37;
	PToken pos TokenRBOpen -> cont 38;
	PToken pos TokenRBClose -> cont 39;
	PToken pos TokenCBOpen -> cont 40;
	PToken pos TokenCBClose -> cont 41;
	PToken pos TokenSBOpen -> cont 42;
	PToken pos TokenSBClose -> cont 43;
	PToken pos TokenLeftarrow -> cont 44;
	PToken pos TokenCompEq -> cont 45;
	PToken pos TokenCompNeq -> cont 46;
	PToken pos TokenCompLt -> cont 47;
	PToken pos TokenCompLeq -> cont 48;
	PToken pos TokenCompGt -> cont 49;
	PToken pos TokenCompGeq -> cont 50;
	PToken pos TokenArithPlus -> cont 51;
	PToken pos TokenArithMinus -> cont 52;
	PToken pos TokenArithMul -> cont 53;
	PToken pos TokenArithDiv -> cont 54;
	PToken pos TokenArithMod -> cont 55;
	PToken pos TokenArithInc -> cont 56;
	PToken pos TokenArithDec -> cont 57;
	PToken pos TokenLogicAnd -> cont 58;
	PToken pos TokenLogicOr -> cont 59;
	PToken pos TokenLogicNot -> cont 60;
	PToken pos (TokenInt happy_dollar_dollar) -> cont 61;
	PToken pos (TokenWord happy_dollar_dollar) -> cont 62;
	PToken pos (TokenStringLit happy_dollar_dollar -> cont 63;
	_ -> happyError' (tk:tks)
	}

happyError_ 64 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => E a -> (a -> E b) -> E b
happyThen = (thenE)
happyReturn :: () => a -> E a
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> E a
happyReturn1 = \a tks -> (returnE) a
happyError' :: () => [(LexToken)] -> E a
happyError' = parseError

parsePSC tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data E a = Ok a | Failed String

thenE :: E a -> (a-> E b) -> E b
m `thenE` k =
    case m of
        Ok a -> k a
        Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
    case m k of
        Ok a -> Ok a
        Failed e -> k e


parseError :: [LexToken] -> a
parseError a = error $ "parse Error: "++ show a
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc-8.0.1/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc640_0/ghc_2.h" #-}


































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

{-# OPTIONS_GHC -w #-}
module Parser where
import Commons
import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,220) ([0,224,32896,1535,0,56,57376,383,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,28672,0,0,0,32768,0,0,0,6144,0,0,0,0,0,0,0,0,32,0,0,0,8,0,0,0,2,0,14336,8192,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,4,0,0,0,1,0,0,16384,0,0,0,4096,0,0,8,0,0,0,0,0,0,14,8,64,32768,3,2,16,0,0,0,0,0,8192,0,0,0,0,16384,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,64,0,0,0,16,0,34560,0,4,14336,8192,0,1,3584,2048,16384,0,896,512,4096,0,224,128,1024,0,56,32,256,0,14,8,64,32768,3,65026,23,0,0,0,0,0,8,0,0,0,2,0,0,32768,0,0,0,6144,0,0,0,0,0,0,0,0,0,0,0,768,16,0,0,34560,0,4,0,0,0,0,0,0,0,0,0,540,4096,0,0,120,0,0,0,768,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,2048,0,0,0,0,0,0,24576,0,480,0,0,0,0,0,0,0,0,0,16384,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,4096,0,0,0,1024,0,0,0,256,0,0,8,0,0,0,0,16,0,1536,0,30,0,384,32768,6,0,96,40960,1,0,24,26624,0,49152,256,0,0,0,0,0,0,28672,8,64,0,7168,2,16,0,32768,0,0,14336,8192,0,1,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,896,512,4096,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,384,32768,7,0,0,0,0,0,512,0,0,0,256,0,0,0,64,0,0,0,0,0,0,0,0,0,57344,32768,0,4,0,16384,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseProgram","Program","TopList","Top","Cmd","AttrDefList","AttrDef","TuplaList","TuplaExp","Tupla","TuplaVal","Expr","BinExpr","JoinExpr","ProdExpr","BaseExpr","AttrList","Cond","Value","seleccion","proyeccion","renombre","union","diferencia","interseccion","producto","division","productoNatural","and","or","not","true","false","'='","'!='","'<'","'>'","'('","')'","'['","']'","','","';'","'->'","':'","quit","help","clear","browse","compile","reload","createRel","insertRel","dropRel","defineOP","null","ident","int","string","%eof"]
        bit_start = st * 62
        bit_end = (st + 1) * 62
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..61]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (22) = happyShift action_10
action_0 (23) = happyShift action_11
action_0 (24) = happyShift action_12
action_0 (40) = happyShift action_13
action_0 (48) = happyShift action_14
action_0 (49) = happyShift action_15
action_0 (50) = happyShift action_16
action_0 (51) = happyShift action_17
action_0 (52) = happyShift action_18
action_0 (53) = happyShift action_19
action_0 (54) = happyShift action_20
action_0 (55) = happyShift action_21
action_0 (56) = happyShift action_22
action_0 (57) = happyShift action_23
action_0 (59) = happyShift action_24
action_0 (4) = happyGoto action_25
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (14) = happyGoto action_5
action_0 (15) = happyGoto action_6
action_0 (16) = happyGoto action_7
action_0 (17) = happyGoto action_8
action_0 (18) = happyGoto action_9
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (22) = happyShift action_10
action_1 (23) = happyShift action_11
action_1 (24) = happyShift action_12
action_1 (40) = happyShift action_13
action_1 (48) = happyShift action_14
action_1 (49) = happyShift action_15
action_1 (50) = happyShift action_16
action_1 (51) = happyShift action_17
action_1 (52) = happyShift action_18
action_1 (53) = happyShift action_19
action_1 (54) = happyShift action_20
action_1 (55) = happyShift action_21
action_1 (56) = happyShift action_22
action_1 (57) = happyShift action_23
action_1 (59) = happyShift action_24
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (14) = happyGoto action_5
action_1 (15) = happyGoto action_6
action_1 (16) = happyGoto action_7
action_1 (17) = happyGoto action_8
action_1 (18) = happyGoto action_9
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (45) = happyShift action_43
action_2 _ = happyReduce_1

action_3 _ = happyReduce_2

action_4 _ = happyReduce_7

action_5 _ = happyReduce_6

action_6 (25) = happyShift action_40
action_6 (26) = happyShift action_41
action_6 (27) = happyShift action_42
action_6 _ = happyReduce_28

action_7 (30) = happyShift action_39
action_7 _ = happyReduce_32

action_8 (28) = happyShift action_37
action_8 (29) = happyShift action_38
action_8 _ = happyReduce_34

action_9 _ = happyReduce_37

action_10 (42) = happyShift action_36
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (42) = happyShift action_35
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (42) = happyShift action_34
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (22) = happyShift action_10
action_13 (23) = happyShift action_11
action_13 (24) = happyShift action_12
action_13 (40) = happyShift action_13
action_13 (59) = happyShift action_33
action_13 (14) = happyGoto action_32
action_13 (15) = happyGoto action_6
action_13 (16) = happyGoto action_7
action_13 (17) = happyGoto action_8
action_13 (18) = happyGoto action_9
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_8

action_15 _ = happyReduce_9

action_16 _ = happyReduce_10

action_17 _ = happyReduce_11

action_18 (61) = happyShift action_31
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_12

action_20 (59) = happyShift action_30
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (59) = happyShift action_29
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (59) = happyShift action_28
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (59) = happyShift action_27
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (36) = happyShift action_26
action_24 _ = happyReduce_42

action_25 (62) = happyAccept
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (22) = happyShift action_10
action_26 (23) = happyShift action_11
action_26 (24) = happyShift action_12
action_26 (40) = happyShift action_13
action_26 (59) = happyShift action_33
action_26 (14) = happyGoto action_68
action_26 (15) = happyGoto action_6
action_26 (16) = happyGoto action_7
action_26 (17) = happyGoto action_8
action_26 (18) = happyGoto action_9
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (22) = happyShift action_10
action_27 (23) = happyShift action_11
action_27 (24) = happyShift action_12
action_27 (40) = happyShift action_13
action_27 (59) = happyShift action_33
action_27 (14) = happyGoto action_67
action_27 (15) = happyGoto action_6
action_27 (16) = happyGoto action_7
action_27 (17) = happyGoto action_8
action_27 (18) = happyGoto action_9
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_16

action_29 (40) = happyShift action_66
action_29 (10) = happyGoto action_64
action_29 (11) = happyGoto action_65
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (59) = happyShift action_63
action_30 (8) = happyGoto action_61
action_30 (9) = happyGoto action_62
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_13

action_32 (41) = happyShift action_60
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_42

action_34 (59) = happyShift action_59
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (59) = happyShift action_58
action_35 (19) = happyGoto action_57
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (33) = happyShift action_52
action_36 (34) = happyShift action_53
action_36 (35) = happyShift action_54
action_36 (40) = happyShift action_55
action_36 (59) = happyShift action_56
action_36 (20) = happyGoto action_51
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (22) = happyShift action_10
action_37 (23) = happyShift action_11
action_37 (24) = happyShift action_12
action_37 (40) = happyShift action_13
action_37 (59) = happyShift action_33
action_37 (18) = happyGoto action_50
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (22) = happyShift action_10
action_38 (23) = happyShift action_11
action_38 (24) = happyShift action_12
action_38 (40) = happyShift action_13
action_38 (59) = happyShift action_33
action_38 (18) = happyGoto action_49
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (22) = happyShift action_10
action_39 (23) = happyShift action_11
action_39 (24) = happyShift action_12
action_39 (40) = happyShift action_13
action_39 (59) = happyShift action_33
action_39 (17) = happyGoto action_48
action_39 (18) = happyGoto action_9
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (22) = happyShift action_10
action_40 (23) = happyShift action_11
action_40 (24) = happyShift action_12
action_40 (40) = happyShift action_13
action_40 (59) = happyShift action_33
action_40 (16) = happyGoto action_47
action_40 (17) = happyGoto action_8
action_40 (18) = happyGoto action_9
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (22) = happyShift action_10
action_41 (23) = happyShift action_11
action_41 (24) = happyShift action_12
action_41 (40) = happyShift action_13
action_41 (59) = happyShift action_33
action_41 (16) = happyGoto action_46
action_41 (17) = happyGoto action_8
action_41 (18) = happyGoto action_9
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (22) = happyShift action_10
action_42 (23) = happyShift action_11
action_42 (24) = happyShift action_12
action_42 (40) = happyShift action_13
action_42 (59) = happyShift action_33
action_42 (16) = happyGoto action_45
action_42 (17) = happyGoto action_8
action_42 (18) = happyGoto action_9
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (22) = happyShift action_10
action_43 (23) = happyShift action_11
action_43 (24) = happyShift action_12
action_43 (40) = happyShift action_13
action_43 (48) = happyShift action_14
action_43 (49) = happyShift action_15
action_43 (50) = happyShift action_16
action_43 (51) = happyShift action_17
action_43 (52) = happyShift action_18
action_43 (53) = happyShift action_19
action_43 (54) = happyShift action_20
action_43 (55) = happyShift action_21
action_43 (56) = happyShift action_22
action_43 (57) = happyShift action_23
action_43 (59) = happyShift action_24
action_43 (6) = happyGoto action_44
action_43 (7) = happyGoto action_4
action_43 (14) = happyGoto action_5
action_43 (15) = happyGoto action_6
action_43 (16) = happyGoto action_7
action_43 (17) = happyGoto action_8
action_43 (18) = happyGoto action_9
action_43 _ = happyReduce_4

action_44 _ = happyReduce_3

action_45 (30) = happyShift action_39
action_45 _ = happyReduce_30

action_46 (30) = happyShift action_39
action_46 _ = happyReduce_31

action_47 (30) = happyShift action_39
action_47 _ = happyReduce_29

action_48 (28) = happyShift action_37
action_48 (29) = happyShift action_38
action_48 _ = happyReduce_33

action_49 _ = happyReduce_36

action_50 _ = happyReduce_35

action_51 (31) = happyShift action_90
action_51 (32) = happyShift action_91
action_51 (43) = happyShift action_92
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (33) = happyShift action_52
action_52 (34) = happyShift action_53
action_52 (35) = happyShift action_54
action_52 (40) = happyShift action_55
action_52 (59) = happyShift action_56
action_52 (20) = happyGoto action_89
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_53

action_54 _ = happyReduce_54

action_55 (33) = happyShift action_52
action_55 (34) = happyShift action_53
action_55 (35) = happyShift action_54
action_55 (40) = happyShift action_55
action_55 (59) = happyShift action_56
action_55 (20) = happyGoto action_88
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (36) = happyShift action_84
action_56 (37) = happyShift action_85
action_56 (38) = happyShift action_86
action_56 (39) = happyShift action_87
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (43) = happyShift action_82
action_57 (44) = happyShift action_83
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_43

action_59 (46) = happyShift action_81
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_41

action_61 (44) = happyShift action_80
action_61 _ = happyReduce_14

action_62 _ = happyReduce_18

action_63 (47) = happyShift action_79
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (44) = happyShift action_78
action_64 _ = happyReduce_15

action_65 _ = happyReduce_21

action_66 (34) = happyShift action_72
action_66 (35) = happyShift action_73
action_66 (58) = happyShift action_74
action_66 (59) = happyShift action_75
action_66 (60) = happyShift action_76
action_66 (61) = happyShift action_77
action_66 (12) = happyGoto action_69
action_66 (13) = happyGoto action_70
action_66 (21) = happyGoto action_71
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_17

action_68 _ = happyReduce_5

action_69 (41) = happyShift action_108
action_69 (44) = happyShift action_109
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_24

action_71 _ = happyReduce_26

action_72 _ = happyReduce_58

action_73 _ = happyReduce_59

action_74 _ = happyReduce_60

action_75 _ = happyReduce_27

action_76 _ = happyReduce_56

action_77 _ = happyReduce_57

action_78 (40) = happyShift action_66
action_78 (11) = happyGoto action_107
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (59) = happyShift action_106
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (59) = happyShift action_63
action_80 (9) = happyGoto action_105
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (59) = happyShift action_104
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (40) = happyShift action_103
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (59) = happyShift action_102
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (34) = happyShift action_72
action_84 (35) = happyShift action_73
action_84 (58) = happyShift action_74
action_84 (59) = happyShift action_101
action_84 (60) = happyShift action_76
action_84 (61) = happyShift action_77
action_84 (21) = happyGoto action_100
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (34) = happyShift action_72
action_85 (35) = happyShift action_73
action_85 (58) = happyShift action_74
action_85 (60) = happyShift action_76
action_85 (61) = happyShift action_77
action_85 (21) = happyGoto action_99
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (34) = happyShift action_72
action_86 (35) = happyShift action_73
action_86 (58) = happyShift action_74
action_86 (60) = happyShift action_76
action_86 (61) = happyShift action_77
action_86 (21) = happyGoto action_98
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (34) = happyShift action_72
action_87 (35) = happyShift action_73
action_87 (58) = happyShift action_74
action_87 (60) = happyShift action_76
action_87 (61) = happyShift action_77
action_87 (21) = happyGoto action_97
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (31) = happyShift action_90
action_88 (32) = happyShift action_91
action_88 (41) = happyShift action_96
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_47

action_90 (33) = happyShift action_52
action_90 (34) = happyShift action_53
action_90 (35) = happyShift action_54
action_90 (40) = happyShift action_55
action_90 (59) = happyShift action_56
action_90 (20) = happyGoto action_95
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (33) = happyShift action_52
action_91 (34) = happyShift action_53
action_91 (35) = happyShift action_54
action_91 (40) = happyShift action_55
action_91 (59) = happyShift action_56
action_91 (20) = happyGoto action_94
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (40) = happyShift action_93
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (22) = happyShift action_10
action_93 (23) = happyShift action_11
action_93 (24) = happyShift action_12
action_93 (40) = happyShift action_13
action_93 (59) = happyShift action_33
action_93 (14) = happyGoto action_113
action_93 (15) = happyGoto action_6
action_93 (16) = happyGoto action_7
action_93 (17) = happyGoto action_8
action_93 (18) = happyGoto action_9
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (31) = happyShift action_90
action_94 _ = happyReduce_46

action_95 _ = happyReduce_45

action_96 _ = happyReduce_55

action_97 _ = happyReduce_52

action_98 _ = happyReduce_51

action_99 _ = happyReduce_50

action_100 _ = happyReduce_49

action_101 _ = happyReduce_48

action_102 _ = happyReduce_44

action_103 (22) = happyShift action_10
action_103 (23) = happyShift action_11
action_103 (24) = happyShift action_12
action_103 (40) = happyShift action_13
action_103 (59) = happyShift action_33
action_103 (14) = happyGoto action_112
action_103 (15) = happyGoto action_6
action_103 (16) = happyGoto action_7
action_103 (17) = happyGoto action_8
action_103 (18) = happyGoto action_9
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (43) = happyShift action_111
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_19

action_106 _ = happyReduce_20

action_107 _ = happyReduce_22

action_108 _ = happyReduce_23

action_109 (34) = happyShift action_72
action_109 (35) = happyShift action_73
action_109 (58) = happyShift action_74
action_109 (59) = happyShift action_75
action_109 (60) = happyShift action_76
action_109 (61) = happyShift action_77
action_109 (13) = happyGoto action_110
action_109 (21) = happyGoto action_71
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_25

action_111 (40) = happyShift action_116
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (41) = happyShift action_115
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (41) = happyShift action_114
action_113 _ = happyFail (happyExpListPerState 113)

action_114 _ = happyReduce_38

action_115 _ = happyReduce_39

action_116 (22) = happyShift action_10
action_116 (23) = happyShift action_11
action_116 (24) = happyShift action_12
action_116 (40) = happyShift action_13
action_116 (59) = happyShift action_33
action_116 (14) = happyGoto action_117
action_116 (15) = happyGoto action_6
action_116 (16) = happyGoto action_7
action_116 (17) = happyGoto action_8
action_116 (18) = happyGoto action_9
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (41) = happyShift action_118
action_117 _ = happyFail (happyExpListPerState 117)

action_118 _ = happyReduce_40

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn14  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn6
		 (TAssign happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn6
		 (TExpr happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (TCmd happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn7
		 (Quit
	)

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn7
		 (Help
	)

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn7
		 (Clear
	)

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn7
		 (Browse
	)

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn7
		 (Reload
	)

happyReduce_13 = happySpecReduce_2  7 happyReduction_13
happyReduction_13 (HappyTerminal (TString happy_var_2))
	_
	 =  HappyAbsSyn7
		 (Compile happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal (TIdentifier happy_var_2))
	_
	 =  HappyAbsSyn7
		 (CreateRel happy_var_2 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_3)
	(HappyTerminal (TIdentifier happy_var_2))
	_
	 =  HappyAbsSyn7
		 (InsertRel happy_var_2 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  7 happyReduction_16
happyReduction_16 (HappyTerminal (TIdentifier happy_var_2))
	_
	 =  HappyAbsSyn7
		 (DropRel happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  7 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (TIdentifier happy_var_2))
	_
	 =  HappyAbsSyn7
		 (DefineOP happy_var_2 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  8 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  8 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  9 happyReduction_20
happyReduction_20 (HappyTerminal (TIdentifier happy_var_3))
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn9
		 ((happy_var_1, parseType happy_var_3)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  10 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  10 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  12 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  13 happyReduction_26
happyReduction_26 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  13 happyReduction_27
happyReduction_27 (HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn13
		 (VString happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  14 happyReduction_28
happyReduction_28 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  15 happyReduction_29
happyReduction_29 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (EUnion happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  15 happyReduction_30
happyReduction_30 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (EInterseccion happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  15 happyReduction_31
happyReduction_31 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (EDiff happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  16 happyReduction_33
happyReduction_33 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (ENaturalJoin happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  16 happyReduction_34
happyReduction_34 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  17 happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (EProd happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (EDiv happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  17 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happyReduce 7 18 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (ESeleccion happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 7 18 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (EProyeccion happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 9 18 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (ERenombre happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_3  18 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  18 happyReduction_42
happyReduction_42 (HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn18
		 (ERelacion happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  19 happyReduction_43
happyReduction_43 (HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  19 happyReduction_44
happyReduction_44 (HappyTerminal (TIdentifier happy_var_3))
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  20 happyReduction_45
happyReduction_45 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (PAnd happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  20 happyReduction_46
happyReduction_46 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (POr  happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  20 happyReduction_47
happyReduction_47 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (PNot happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  20 happyReduction_48
happyReduction_48 (HappyTerminal (TIdentifier happy_var_3))
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn20
		 (PAttrEq happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  20 happyReduction_49
happyReduction_49 (HappyAbsSyn21  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn20
		 (PEq  happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  20 happyReduction_50
happyReduction_50 (HappyAbsSyn21  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn20
		 (PNeq happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  20 happyReduction_51
happyReduction_51 (HappyAbsSyn21  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn20
		 (PLt  happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  20 happyReduction_52
happyReduction_52 (HappyAbsSyn21  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn20
		 (PGt  happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  20 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn20
		 (PTrue
	)

happyReduce_54 = happySpecReduce_1  20 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn20
		 (PFalse
	)

happyReduce_55 = happySpecReduce_3  20 happyReduction_55
happyReduction_55 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  21 happyReduction_56
happyReduction_56 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn21
		 (VInt happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  21 happyReduction_57
happyReduction_57 (HappyTerminal (TString happy_var_1))
	 =  HappyAbsSyn21
		 (VString happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  21 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn21
		 (VBool True
	)

happyReduce_59 = happySpecReduce_1  21 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn21
		 (VBool False
	)

happyReduce_60 = happySpecReduce_1  21 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn21
		 (VNull
	)

happyNewToken action sts stk [] =
	action 62 62 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TSelect -> cont 22;
	TProject -> cont 23;
	TRename -> cont 24;
	TUnion -> cont 25;
	TDiferencia -> cont 26;
	TInterseccion -> cont 27;
	TProducto -> cont 28;
	TDivision -> cont 29;
	TNaturalJoin -> cont 30;
	TAnd -> cont 31;
	TOr -> cont 32;
	TNot -> cont 33;
	TTrue -> cont 34;
	TFalse -> cont 35;
	TEq -> cont 36;
	TNeq -> cont 37;
	TLt -> cont 38;
	TGt -> cont 39;
	TLParen -> cont 40;
	TRParen -> cont 41;
	TLBracket -> cont 42;
	TRBracket -> cont 43;
	TComma -> cont 44;
	TSemicolon -> cont 45;
	TArrow -> cont 46;
	TColon -> cont 47;
	TQuit -> cont 48;
	THelp -> cont 49;
	TClear -> cont 50;
	TBrowse -> cont 51;
	TCompile -> cont 52;
	TReload -> cont 53;
	TCreateRel -> cont 54;
	TInsertRel -> cont 55;
	TDropRel -> cont 56;
	TDefineOP -> cont 57;
	TNull -> cont 58;
	TIdentifier happy_dollar_dollar -> cont 59;
	TInt happy_dollar_dollar -> cont 60;
	TString happy_dollar_dollar -> cont 61;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 62 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = (>>=)
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Either String a
happyError' = (\(tokens, _) -> parseError tokens)
parseProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- -------------------------------------------------------
-- Parsers Auxiliares
-- -------------------------------------------------------


parseType :: String -> Type
parseType s = case map toLower s of
  "int"    -> PInt
  "string" -> PString
  "bool"   -> PBool
  _        -> error ("Tipo desconocido: " ++ s)

parseError :: [Token] -> Either String a
parseError [] =
  Left "Error de sintaxis: fin inesperado de la entrada. Revisa si faltan cerrar paréntesis o comillas."
parseError (tok:_) =
  let hint = case tok of
               TColon -> ". Comando no reconocido. Para ver los comandos disponibles ingrese: \':help\'" 
               _      -> ""
  in Left $ "Error de sintaxis cerca de " ++ showToken tok ++ hint

showToken :: Token -> String
showToken t = case t of
    TSelect       -> "la palabra 'seleccion'"
    TProject      -> "la palabra 'proyeccion'"
    TUnion        -> "la palabra 'union'"
    TDiferencia   -> "la palabra 'diferencia'"
    TInterseccion -> "la palabra 'interseccion'"
    TProducto     -> "la palabra 'producto'"
    TNaturalJoin  -> "la palabra 'productoNatural'"
    TDivision     -> "la palabra 'division'"
    TRename       -> "la palabra 'renombre'"
    TAnd          -> "el operador 'and'"
    TOr           -> "el operador 'or'"
    TNot          -> "el operador 'not'"
    TTrue         -> "el valor 'true'"
    TFalse        -> "el valor 'false'"
    TNull         -> "el valor 'null'"
    TEq           -> "el signo '='"
    TNeq          -> "el signo '!='"
    TLt           -> "el signo '<'"
    TGt           -> "el signo '>'"
    TLParen       -> "el paréntesis de apertura '('"
    TRParen       -> "el paréntesis de cierre ')'"
    TLBracket     -> "el corchete '['"
    TRBracket     -> "el corchete ']'"
    TComma        -> "la coma ','"
    TSemicolon    -> "el punto y coma ';'"
    TArrow        -> "la flecha '->'"
    TColon        -> "los dos puntos ':'"
    TIdentifier s -> "el identificador '" ++ s ++ "'"
    TInt n        -> "el número '" ++ show n ++ "'"
    TString s     -> "el texto \"" ++ s ++ "\""
    TQuit         -> "el comando ':quit'"
    THelp         -> "el comando ':help'"
    TCompile      -> "el comando ':compile'"
    TReload       -> "el comando ':reload'"
    TCreateRel    -> "el comando ':createRel'"
    TInsertRel    -> "el comando ':insertRel'"
    TDropRel      -> "el comando ':dropRel'"
    TDefineOP     -> "el comando ':defineOP'"
    _             -> "el símbolo desconocido (" ++ show t ++ ")"

-- -------------------------------------------------------
-- Tokens
-- -------------------------------------------------------
data Token
    = TSelect
    | TProject
    | TRename
    | TUnion
    | TDiferencia
    | TInterseccion
    | TProducto
    | TDivision
    | TNaturalJoin
    | TAnd
    | TOr
    | TNot
    | TTrue
    | TFalse
    | TEq
    | TNeq
    | TLt
    | TGt
    | TLParen
    | TRParen
    | TLBracket
    | TRBracket
    | TComma
    | TSemicolon
    | TArrow
    | TColon
    -- Comandos REPL:
    | TQuit
    | THelp
    | TClear
    | TBrowse
    | TCompile
    | TReload
    | TCreateRel
    | TInsertRel
    | TDropRel
    | TDefineOP
    -- Literales:
    | TNull
    | TIdentifier String
    | TInt Int
    | TString String
    deriving (Show, Eq)

-- -------------------------------------------------------
-- Lexer
-- -------------------------------------------------------
lexer :: String -> Either Err [Token]
lexer [] = Right []
lexer (c:cs)
  | isSpace c = lexer cs
  | c == '('  = add TLParen
  | c == ')'  = add TRParen
  | c == '['  = add TLBracket
  | c == ']'  = add TRBracket
  | c == ','  = add TComma
  | c == ';'  = add TSemicolon
  | c == '='  = add TEq
  | c == '<'  = add TLt
  | c == '>'  = add TGt

  | c == '-' && not (null cs) && head cs == '>'
      = prepend TArrow (tail cs)

  -- Para trabajar numeros negativos:  
  | c == '-' && not (null cs) && isDigit (head cs) =
      let (num, rest) = span isDigit cs
      in prepend (TInt (read ("-" ++ num))) rest


  | c == '!' && not (null cs) && head cs == '='
      = prepend TNeq (tail cs)

  -- ':' seguido de letras → comando REPL si es conocido, sino TColon + keyword
  | c == ':' && not (null cs) && isAlpha (head cs) =
      let (word, rest) = span isAlphaNum cs
      in case keywordCmd word of
           Just tok -> prepend tok rest
           Nothing  -> case lexer rest of
                         Left err -> Left err
                         Right ts -> Right (TColon : keyword word : ts)

  -- ':' solo → separador atrib:tipo
  | c == ':' = add TColon

  -- Comentarios de línea:
  | c == '/' && not (null cs) && head cs == '/'
      = lexer (dropWhile (/= '\n') cs)

  | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in prepend (TInt (read num)) rest

  | c == '"' =
      case span (/= '"') cs of
        (str, '"':rest) -> prepend (TString str) rest
        _               -> Left "String sin cerrar"

  | isAlpha c || c == '_' =
      let (word, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
      in prepend (keyword word) rest

  | otherwise = Left ("Caracter inesperado: " ++ [c])
  where
    add tok     = prepend tok cs
    prepend tok rest =
      case lexer rest of
        Left err -> Left err
        Right ts -> Right (tok : ts)

keyword :: String -> Token
keyword w = case w of
    "seleccion"       -> TSelect
    "proyeccion"      -> TProject
    "renombre"        -> TRename
    "union"           -> TUnion
    "diferencia"      -> TDiferencia
    "interseccion"    -> TInterseccion
    "producto"        -> TProducto
    "division"        -> TDivision
    "productoNatural" -> TNaturalJoin
    "and"             -> TAnd
    "or"              -> TOr
    "not"             -> TNot
    "true"            -> TTrue
    "false"           -> TFalse
    "null"            -> TNull
    _                 -> TIdentifier w

-- El ':' ya fue consumido, recibe solo la palabra:
keywordCmd :: String -> Maybe Token
keywordCmd w = case w of
    "quit"      -> Just TQuit
    "q"         -> Just TQuit
    "help"      -> Just THelp
    "h"         -> Just THelp
    "clear"     -> Just TClear
    "browse"    -> Just TBrowse
    "compile"   -> Just TCompile
    "load"      -> Just TCompile
    "l"         -> Just TCompile
    "c"         -> Just TCompile
    "reload"    -> Just TReload
    "r"         -> Just TReload
    "createRel" -> Just TCreateRel
    "insertRel" -> Just TInsertRel
    "dropRel"   -> Just TDropRel
    "defineOP"  -> Just TDefineOP
    _           -> Nothing

-- -------------------------------------------------------
-- Funciones exportadas
-- -------------------------------------------------------
parse' :: String -> Either String [TopLevel]
parse' input = do
  toks <- lexer input
  parseProgram toks

parse :: String -> Either String Expr
parse input = do
  tops <- parse' input
  case tops of
    [TExpr e] -> return e
    []        -> Left "Entrada vacía"
    _         -> Left "Se esperaba una única expresión"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






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
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

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

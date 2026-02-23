{-# OPTIONS_GHC -w #-}
module Parser where
import AST
import Token
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
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

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,194) ([57344,1,16448,32768,7,256,1,224,0,0,12288,0,0,12288,0,0,0,0,0,0,0,16,0,0,64,0,0,256,0,0,1024,0,30,1024,4,0,0,0,0,0,0,0,0,2,0,0,1024,0,0,4096,0,0,16384,0,28672,256,1,30,1024,4,120,4096,16,480,16384,64,0,0,4,7680,0,1028,30720,0,4112,57344,1,16448,0,768,0,0,3072,0,0,12288,0,0,0,16412,64,49152,0,0,0,0,0,0,0,0,0,768,512,0,28672,256,1,0,0,0,0,0,0,0,16412,64,0,1920,0,0,0,96,0,0,0,0,0,4096,0,0,12288,0,0,0,0,0,0,16,0,15872,0,0,0,256,0,0,4,0,1536,30720,0,6144,40960,1,24576,32768,6,32768,1,26,49152,8192,0,0,0,0,0,112,257,0,448,1028,0,0,16,0,768,512,32768,7,256,1,30,1024,4,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57344,1,16448,0,0,2048,0,0,24576,0,0,0,0,0,16384,0,0,0,1,0,0,4,0,0,16,0,0,64,0,0,0,0,0,0,4,0,0,16,0,0,64,0,0,256,0,0,1024,0,0,16,0,0,62,0,0,256,0,0,2048,0,0,8192,0,12288,0,0,0,0,0,0,0,0,30720,0,4112,0,0,0,32768,7,256,1,0,2048,0,0,8192,0,0,32768,0,0,0,2,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,2,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseExpr","Expr","BinExpr","JoinExpr","ProdExpr","BaseExpr","AttrList","AggList","Agg","Cond","Value","select","project","rename","group","union","diferencia","intersec","producto","division","naturaljoin","join","and","or","not","true","false","'='","'!='","'<'","'>'","count","sum","avg","min","max","'('","')'","'['","']'","','","';'","'->'","null","ident","int","string","%eof"]
        bit_start = st * 50
        bit_end = (st + 1) * 50
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..49]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (14) = happyShift action_6
action_0 (15) = happyShift action_7
action_0 (16) = happyShift action_8
action_0 (17) = happyShift action_9
action_0 (39) = happyShift action_10
action_0 (47) = happyShift action_11
action_0 (4) = happyGoto action_12
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (14) = happyShift action_6
action_1 (15) = happyShift action_7
action_1 (16) = happyShift action_8
action_1 (17) = happyShift action_9
action_1 (39) = happyShift action_10
action_1 (47) = happyShift action_11
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (18) = happyShift action_22
action_2 (19) = happyShift action_23
action_2 (20) = happyShift action_24
action_2 _ = happyReduce_1

action_3 (23) = happyShift action_20
action_3 (24) = happyShift action_21
action_3 _ = happyReduce_5

action_4 (21) = happyShift action_18
action_4 (22) = happyShift action_19
action_4 _ = happyReduce_8

action_5 _ = happyReduce_11

action_6 (41) = happyShift action_17
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (41) = happyShift action_16
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (41) = happyShift action_15
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (41) = happyShift action_14
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (14) = happyShift action_6
action_10 (15) = happyShift action_7
action_10 (16) = happyShift action_8
action_10 (17) = happyShift action_9
action_10 (39) = happyShift action_10
action_10 (47) = happyShift action_11
action_10 (4) = happyGoto action_13
action_10 (5) = happyGoto action_2
action_10 (6) = happyGoto action_3
action_10 (7) = happyGoto action_4
action_10 (8) = happyGoto action_5
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_17

action_12 (50) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (40) = happyShift action_42
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (47) = happyShift action_39
action_14 (9) = happyGoto action_41
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (47) = happyShift action_40
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (47) = happyShift action_39
action_16 (9) = happyGoto action_38
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (27) = happyShift action_33
action_17 (28) = happyShift action_34
action_17 (29) = happyShift action_35
action_17 (39) = happyShift action_36
action_17 (47) = happyShift action_37
action_17 (12) = happyGoto action_32
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (14) = happyShift action_6
action_18 (15) = happyShift action_7
action_18 (16) = happyShift action_8
action_18 (17) = happyShift action_9
action_18 (39) = happyShift action_10
action_18 (47) = happyShift action_11
action_18 (8) = happyGoto action_31
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (14) = happyShift action_6
action_19 (15) = happyShift action_7
action_19 (16) = happyShift action_8
action_19 (17) = happyShift action_9
action_19 (39) = happyShift action_10
action_19 (47) = happyShift action_11
action_19 (8) = happyGoto action_30
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (14) = happyShift action_6
action_20 (15) = happyShift action_7
action_20 (16) = happyShift action_8
action_20 (17) = happyShift action_9
action_20 (39) = happyShift action_10
action_20 (47) = happyShift action_11
action_20 (7) = happyGoto action_29
action_20 (8) = happyGoto action_5
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (41) = happyShift action_28
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (14) = happyShift action_6
action_22 (15) = happyShift action_7
action_22 (16) = happyShift action_8
action_22 (17) = happyShift action_9
action_22 (39) = happyShift action_10
action_22 (47) = happyShift action_11
action_22 (6) = happyGoto action_27
action_22 (7) = happyGoto action_4
action_22 (8) = happyGoto action_5
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (14) = happyShift action_6
action_23 (15) = happyShift action_7
action_23 (16) = happyShift action_8
action_23 (17) = happyShift action_9
action_23 (39) = happyShift action_10
action_23 (47) = happyShift action_11
action_23 (6) = happyGoto action_26
action_23 (7) = happyGoto action_4
action_23 (8) = happyGoto action_5
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (14) = happyShift action_6
action_24 (15) = happyShift action_7
action_24 (16) = happyShift action_8
action_24 (17) = happyShift action_9
action_24 (39) = happyShift action_10
action_24 (47) = happyShift action_11
action_24 (6) = happyGoto action_25
action_24 (7) = happyGoto action_4
action_24 (8) = happyGoto action_5
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (23) = happyShift action_20
action_25 (24) = happyShift action_21
action_25 _ = happyReduce_3

action_26 (23) = happyShift action_20
action_26 (24) = happyShift action_21
action_26 _ = happyReduce_4

action_27 (23) = happyShift action_20
action_27 (24) = happyShift action_21
action_27 _ = happyReduce_2

action_28 (27) = happyShift action_33
action_28 (28) = happyShift action_34
action_28 (29) = happyShift action_35
action_28 (39) = happyShift action_36
action_28 (47) = happyShift action_37
action_28 (12) = happyGoto action_56
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (21) = happyShift action_18
action_29 (22) = happyShift action_19
action_29 _ = happyReduce_6

action_30 _ = happyReduce_10

action_31 _ = happyReduce_9

action_32 (25) = happyShift action_53
action_32 (26) = happyShift action_54
action_32 (42) = happyShift action_55
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (27) = happyShift action_33
action_33 (28) = happyShift action_34
action_33 (29) = happyShift action_35
action_33 (39) = happyShift action_36
action_33 (47) = happyShift action_37
action_33 (12) = happyGoto action_52
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_35

action_35 _ = happyReduce_36

action_36 (27) = happyShift action_33
action_36 (28) = happyShift action_34
action_36 (29) = happyShift action_35
action_36 (39) = happyShift action_36
action_36 (47) = happyShift action_37
action_36 (12) = happyGoto action_51
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (30) = happyShift action_47
action_37 (31) = happyShift action_48
action_37 (32) = happyShift action_49
action_37 (33) = happyShift action_50
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (42) = happyShift action_46
action_38 (43) = happyShift action_43
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_18

action_40 (45) = happyShift action_45
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (43) = happyShift action_43
action_41 (44) = happyShift action_44
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_16

action_43 (47) = happyShift action_81
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (34) = happyShift action_76
action_44 (35) = happyShift action_77
action_44 (36) = happyShift action_78
action_44 (37) = happyShift action_79
action_44 (38) = happyShift action_80
action_44 (10) = happyGoto action_74
action_44 (11) = happyGoto action_75
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (47) = happyShift action_73
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (39) = happyShift action_72
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (28) = happyShift action_63
action_47 (29) = happyShift action_64
action_47 (46) = happyShift action_65
action_47 (47) = happyShift action_71
action_47 (48) = happyShift action_66
action_47 (49) = happyShift action_67
action_47 (13) = happyGoto action_70
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (28) = happyShift action_63
action_48 (29) = happyShift action_64
action_48 (46) = happyShift action_65
action_48 (48) = happyShift action_66
action_48 (49) = happyShift action_67
action_48 (13) = happyGoto action_69
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (28) = happyShift action_63
action_49 (29) = happyShift action_64
action_49 (46) = happyShift action_65
action_49 (48) = happyShift action_66
action_49 (49) = happyShift action_67
action_49 (13) = happyGoto action_68
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (28) = happyShift action_63
action_50 (29) = happyShift action_64
action_50 (46) = happyShift action_65
action_50 (48) = happyShift action_66
action_50 (49) = happyShift action_67
action_50 (13) = happyGoto action_62
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (25) = happyShift action_53
action_51 (26) = happyShift action_54
action_51 (40) = happyShift action_61
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_29

action_53 (27) = happyShift action_33
action_53 (28) = happyShift action_34
action_53 (29) = happyShift action_35
action_53 (39) = happyShift action_36
action_53 (47) = happyShift action_37
action_53 (12) = happyGoto action_60
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (27) = happyShift action_33
action_54 (28) = happyShift action_34
action_54 (29) = happyShift action_35
action_54 (39) = happyShift action_36
action_54 (47) = happyShift action_37
action_54 (12) = happyGoto action_59
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (39) = happyShift action_58
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (25) = happyShift action_53
action_56 (26) = happyShift action_54
action_56 (42) = happyShift action_57
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (14) = happyShift action_6
action_57 (15) = happyShift action_7
action_57 (16) = happyShift action_8
action_57 (17) = happyShift action_9
action_57 (39) = happyShift action_10
action_57 (47) = happyShift action_11
action_57 (7) = happyGoto action_92
action_57 (8) = happyGoto action_5
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (14) = happyShift action_6
action_58 (15) = happyShift action_7
action_58 (16) = happyShift action_8
action_58 (17) = happyShift action_9
action_58 (39) = happyShift action_10
action_58 (47) = happyShift action_11
action_58 (4) = happyGoto action_91
action_58 (5) = happyGoto action_2
action_58 (6) = happyGoto action_3
action_58 (7) = happyGoto action_4
action_58 (8) = happyGoto action_5
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (25) = happyShift action_53
action_59 _ = happyReduce_28

action_60 _ = happyReduce_27

action_61 _ = happyReduce_37

action_62 _ = happyReduce_34

action_63 _ = happyReduce_40

action_64 _ = happyReduce_41

action_65 _ = happyReduce_42

action_66 _ = happyReduce_38

action_67 _ = happyReduce_39

action_68 _ = happyReduce_33

action_69 _ = happyReduce_32

action_70 _ = happyReduce_31

action_71 _ = happyReduce_30

action_72 (14) = happyShift action_6
action_72 (15) = happyShift action_7
action_72 (16) = happyShift action_8
action_72 (17) = happyShift action_9
action_72 (39) = happyShift action_10
action_72 (47) = happyShift action_11
action_72 (4) = happyGoto action_90
action_72 (5) = happyGoto action_2
action_72 (6) = happyGoto action_3
action_72 (7) = happyGoto action_4
action_72 (8) = happyGoto action_5
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (42) = happyShift action_89
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (42) = happyShift action_87
action_74 (43) = happyShift action_88
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_20

action_76 (39) = happyShift action_86
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (39) = happyShift action_85
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (39) = happyShift action_84
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (39) = happyShift action_83
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (39) = happyShift action_82
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_19

action_82 (47) = happyShift action_102
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (47) = happyShift action_101
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (47) = happyShift action_100
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (47) = happyShift action_99
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (47) = happyShift action_98
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (39) = happyShift action_97
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (34) = happyShift action_76
action_88 (35) = happyShift action_77
action_88 (36) = happyShift action_78
action_88 (37) = happyShift action_79
action_88 (38) = happyShift action_80
action_88 (11) = happyGoto action_96
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (39) = happyShift action_95
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (40) = happyShift action_94
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (40) = happyShift action_93
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (21) = happyShift action_18
action_92 (22) = happyShift action_19
action_92 _ = happyReduce_7

action_93 _ = happyReduce_12

action_94 _ = happyReduce_13

action_95 (14) = happyShift action_6
action_95 (15) = happyShift action_7
action_95 (16) = happyShift action_8
action_95 (17) = happyShift action_9
action_95 (39) = happyShift action_10
action_95 (47) = happyShift action_11
action_95 (4) = happyGoto action_109
action_95 (5) = happyGoto action_2
action_95 (6) = happyGoto action_3
action_95 (7) = happyGoto action_4
action_95 (8) = happyGoto action_5
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_21

action_97 (14) = happyShift action_6
action_97 (15) = happyShift action_7
action_97 (16) = happyShift action_8
action_97 (17) = happyShift action_9
action_97 (39) = happyShift action_10
action_97 (47) = happyShift action_11
action_97 (4) = happyGoto action_108
action_97 (5) = happyGoto action_2
action_97 (6) = happyGoto action_3
action_97 (7) = happyGoto action_4
action_97 (8) = happyGoto action_5
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (40) = happyShift action_107
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (40) = happyShift action_106
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (40) = happyShift action_105
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (40) = happyShift action_104
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (40) = happyShift action_103
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_26

action_104 _ = happyReduce_25

action_105 _ = happyReduce_24

action_106 _ = happyReduce_23

action_107 _ = happyReduce_22

action_108 (40) = happyShift action_111
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (40) = happyShift action_110
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_14

action_111 _ = happyReduce_15

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EUnion happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EInterseccion happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EDiff happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (ENaturalJoin happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 6 6 happyReduction_7
happyReduction_7 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (EJoin happy_var_4 happy_var_1 happy_var_6
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (EProd happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (EDiv happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happyReduce 7 8 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (ESeleccion happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 7 8 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (EProyeccion happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 9 8 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (ERenombre happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 9 8 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (EGroup happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  8 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  8 happyReduction_17
happyReduction_17 (HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn8
		 (ERelacion happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 (HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 (HappyTerminal (TIdentifier happy_var_3))
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  10 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  10 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 11 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((Count, happy_var_3)
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 11 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((Sum, happy_var_3)
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 11 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((Avg, happy_var_3)
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 4 11 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((Min, happy_var_3)
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 11 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((Max, happy_var_3)
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_3  12 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (PAnd happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  12 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (POr happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  12 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (PNot happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  12 happyReduction_30
happyReduction_30 (HappyTerminal (TIdentifier happy_var_3))
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn12
		 (PAttrEq happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  12 happyReduction_31
happyReduction_31 (HappyAbsSyn13  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn12
		 (PEq  happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  12 happyReduction_32
happyReduction_32 (HappyAbsSyn13  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn12
		 (PNeq happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  12 happyReduction_33
happyReduction_33 (HappyAbsSyn13  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn12
		 (PLt  happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  12 happyReduction_34
happyReduction_34 (HappyAbsSyn13  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn12
		 (PGt  happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  12 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn12
		 (PTrue
	)

happyReduce_36 = happySpecReduce_1  12 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn12
		 (PFalse
	)

happyReduce_37 = happySpecReduce_3  12 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  13 happyReduction_38
happyReduction_38 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn13
		 (VInt happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  13 happyReduction_39
happyReduction_39 (HappyTerminal (TString happy_var_1))
	 =  HappyAbsSyn13
		 (VString happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  13 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn13
		 (VBool True
	)

happyReduce_41 = happySpecReduce_1  13 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn13
		 (VBool False
	)

happyReduce_42 = happySpecReduce_1  13 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn13
		 (VNull
	)

happyNewToken action sts stk [] =
	action 50 50 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TSelect -> cont 14;
	TProject -> cont 15;
	TRename -> cont 16;
	TGroup -> cont 17;
	TUnion -> cont 18;
	TDiferencia -> cont 19;
	TInterseccion -> cont 20;
	TProducto -> cont 21;
	TDivision -> cont 22;
	TNaturalJoin -> cont 23;
	TJoin -> cont 24;
	TAnd -> cont 25;
	TOr -> cont 26;
	TNot -> cont 27;
	TTrue -> cont 28;
	TFalse -> cont 29;
	TEq -> cont 30;
	TNeq -> cont 31;
	TLt -> cont 32;
	TGt -> cont 33;
	TCount -> cont 34;
	TSum -> cont 35;
	TAvg -> cont 36;
	TMin -> cont 37;
	TMax -> cont 38;
	TLParen -> cont 39;
	TRParen -> cont 40;
	TLBracket -> cont 41;
	TRBracket -> cont 42;
	TComma -> cont 43;
	TSemicolon -> cont 44;
	TArrow -> cont 45;
	TNull -> cont 46;
	TIdentifier happy_dollar_dollar -> cont 47;
	TInt happy_dollar_dollar -> cont 48;
	TString happy_dollar_dollar -> cont 49;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 50 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parseExpr tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError tokens = error ("Error en parser: " ++ show tokens)
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

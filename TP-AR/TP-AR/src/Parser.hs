{-# OPTIONS_GHC -w #-}
module Parser where
import Commons
import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
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

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,135) ([14336,8192,16,56,4128,49152,1,0,2048,0,0,6,0,0,0,0,32768,0,0,128,0,32768,0,56,4128,0,0,0,0,0,0,16384,0,0,4096,0,0,16,49152,4129,14336,8192,16,56,4128,14336,8192,16,56,4128,14336,8192,16,56,4128,0,8,0,2048,0,0,8,0,1536,0,0,0,0,0,0,0,48,1,49152,4129,0,0,0,0,0,0,8640,16,0,30,0,0,3,0,0,0,0,4,0,0,0,0,16,0,32,0,0,16,32768,30721,0,384,104,32768,26625,0,384,104,12288,64,0,0,0,49152,4129,0,8640,16,0,32,14336,8192,16,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,56,4128,0,0,1,0,32,0,16384,0,0,64,0,0,0,0,0,14336,8192,16,0,64,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseExpr","Expr","BinExpr","JoinExpr","ProdExpr","BaseExpr","AttrList","Cond","Value","seleccion","proyeccion","renombre","union","diferencia","interseccion","producto","division","naturaljoin","and","or","not","true","false","'='","'!='","'<'","'>'","'('","')'","'['","']'","','","'->'","null","ident","int","string","%eof"]
        bit_start = st * 40
        bit_end = (st + 1) * 40
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..39]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (12) = happyShift action_6
action_0 (13) = happyShift action_7
action_0 (14) = happyShift action_8
action_0 (30) = happyShift action_9
action_0 (37) = happyShift action_10
action_0 (4) = happyGoto action_11
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (12) = happyShift action_6
action_1 (13) = happyShift action_7
action_1 (14) = happyShift action_8
action_1 (30) = happyShift action_9
action_1 (37) = happyShift action_10
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (15) = happyShift action_19
action_2 (16) = happyShift action_20
action_2 (17) = happyShift action_21
action_2 _ = happyReduce_1

action_3 (20) = happyShift action_18
action_3 _ = happyReduce_5

action_4 (18) = happyShift action_16
action_4 (19) = happyShift action_17
action_4 _ = happyReduce_7

action_5 _ = happyReduce_10

action_6 (32) = happyShift action_15
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (32) = happyShift action_14
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (32) = happyShift action_13
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (12) = happyShift action_6
action_9 (13) = happyShift action_7
action_9 (14) = happyShift action_8
action_9 (30) = happyShift action_9
action_9 (37) = happyShift action_10
action_9 (4) = happyGoto action_12
action_9 (5) = happyGoto action_2
action_9 (6) = happyGoto action_3
action_9 (7) = happyGoto action_4
action_9 (8) = happyGoto action_5
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_15

action_11 (40) = happyAccept
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (31) = happyShift action_37
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (37) = happyShift action_36
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (37) = happyShift action_35
action_14 (9) = happyGoto action_34
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (23) = happyShift action_29
action_15 (24) = happyShift action_30
action_15 (25) = happyShift action_31
action_15 (30) = happyShift action_32
action_15 (37) = happyShift action_33
action_15 (10) = happyGoto action_28
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (12) = happyShift action_6
action_16 (13) = happyShift action_7
action_16 (14) = happyShift action_8
action_16 (30) = happyShift action_9
action_16 (37) = happyShift action_10
action_16 (8) = happyGoto action_27
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (12) = happyShift action_6
action_17 (13) = happyShift action_7
action_17 (14) = happyShift action_8
action_17 (30) = happyShift action_9
action_17 (37) = happyShift action_10
action_17 (8) = happyGoto action_26
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (12) = happyShift action_6
action_18 (13) = happyShift action_7
action_18 (14) = happyShift action_8
action_18 (30) = happyShift action_9
action_18 (37) = happyShift action_10
action_18 (7) = happyGoto action_25
action_18 (8) = happyGoto action_5
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (12) = happyShift action_6
action_19 (13) = happyShift action_7
action_19 (14) = happyShift action_8
action_19 (30) = happyShift action_9
action_19 (37) = happyShift action_10
action_19 (6) = happyGoto action_24
action_19 (7) = happyGoto action_4
action_19 (8) = happyGoto action_5
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (12) = happyShift action_6
action_20 (13) = happyShift action_7
action_20 (14) = happyShift action_8
action_20 (30) = happyShift action_9
action_20 (37) = happyShift action_10
action_20 (6) = happyGoto action_23
action_20 (7) = happyGoto action_4
action_20 (8) = happyGoto action_5
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (12) = happyShift action_6
action_21 (13) = happyShift action_7
action_21 (14) = happyShift action_8
action_21 (30) = happyShift action_9
action_21 (37) = happyShift action_10
action_21 (6) = happyGoto action_22
action_21 (7) = happyGoto action_4
action_21 (8) = happyGoto action_5
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (20) = happyShift action_18
action_22 _ = happyReduce_3

action_23 (20) = happyShift action_18
action_23 _ = happyReduce_4

action_24 (20) = happyShift action_18
action_24 _ = happyReduce_2

action_25 (18) = happyShift action_16
action_25 (19) = happyShift action_17
action_25 _ = happyReduce_6

action_26 _ = happyReduce_9

action_27 _ = happyReduce_8

action_28 (21) = happyShift action_47
action_28 (22) = happyShift action_48
action_28 (33) = happyShift action_49
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (23) = happyShift action_29
action_29 (24) = happyShift action_30
action_29 (25) = happyShift action_31
action_29 (30) = happyShift action_32
action_29 (37) = happyShift action_33
action_29 (10) = happyGoto action_46
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_26

action_31 _ = happyReduce_27

action_32 (23) = happyShift action_29
action_32 (24) = happyShift action_30
action_32 (25) = happyShift action_31
action_32 (30) = happyShift action_32
action_32 (37) = happyShift action_33
action_32 (10) = happyGoto action_45
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (26) = happyShift action_41
action_33 (27) = happyShift action_42
action_33 (28) = happyShift action_43
action_33 (29) = happyShift action_44
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (33) = happyShift action_39
action_34 (34) = happyShift action_40
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_16

action_36 (35) = happyShift action_38
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_14

action_38 (37) = happyShift action_66
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (30) = happyShift action_65
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (37) = happyShift action_64
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (24) = happyShift action_55
action_41 (25) = happyShift action_56
action_41 (36) = happyShift action_57
action_41 (37) = happyShift action_63
action_41 (38) = happyShift action_58
action_41 (39) = happyShift action_59
action_41 (11) = happyGoto action_62
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (24) = happyShift action_55
action_42 (25) = happyShift action_56
action_42 (36) = happyShift action_57
action_42 (38) = happyShift action_58
action_42 (39) = happyShift action_59
action_42 (11) = happyGoto action_61
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (24) = happyShift action_55
action_43 (25) = happyShift action_56
action_43 (36) = happyShift action_57
action_43 (38) = happyShift action_58
action_43 (39) = happyShift action_59
action_43 (11) = happyGoto action_60
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (24) = happyShift action_55
action_44 (25) = happyShift action_56
action_44 (36) = happyShift action_57
action_44 (38) = happyShift action_58
action_44 (39) = happyShift action_59
action_44 (11) = happyGoto action_54
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (21) = happyShift action_47
action_45 (22) = happyShift action_48
action_45 (31) = happyShift action_53
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_20

action_47 (23) = happyShift action_29
action_47 (24) = happyShift action_30
action_47 (25) = happyShift action_31
action_47 (30) = happyShift action_32
action_47 (37) = happyShift action_33
action_47 (10) = happyGoto action_52
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (23) = happyShift action_29
action_48 (24) = happyShift action_30
action_48 (25) = happyShift action_31
action_48 (30) = happyShift action_32
action_48 (37) = happyShift action_33
action_48 (10) = happyGoto action_51
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (30) = happyShift action_50
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (12) = happyShift action_6
action_50 (13) = happyShift action_7
action_50 (14) = happyShift action_8
action_50 (30) = happyShift action_9
action_50 (37) = happyShift action_10
action_50 (4) = happyGoto action_69
action_50 (5) = happyGoto action_2
action_50 (6) = happyGoto action_3
action_50 (7) = happyGoto action_4
action_50 (8) = happyGoto action_5
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (21) = happyShift action_47
action_51 _ = happyReduce_19

action_52 _ = happyReduce_18

action_53 _ = happyReduce_28

action_54 _ = happyReduce_25

action_55 _ = happyReduce_31

action_56 _ = happyReduce_32

action_57 _ = happyReduce_33

action_58 _ = happyReduce_29

action_59 _ = happyReduce_30

action_60 _ = happyReduce_24

action_61 _ = happyReduce_23

action_62 _ = happyReduce_22

action_63 _ = happyReduce_21

action_64 _ = happyReduce_17

action_65 (12) = happyShift action_6
action_65 (13) = happyShift action_7
action_65 (14) = happyShift action_8
action_65 (30) = happyShift action_9
action_65 (37) = happyShift action_10
action_65 (4) = happyGoto action_68
action_65 (5) = happyGoto action_2
action_65 (6) = happyGoto action_3
action_65 (7) = happyGoto action_4
action_65 (8) = happyGoto action_5
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (33) = happyShift action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (30) = happyShift action_72
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (31) = happyShift action_71
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (31) = happyShift action_70
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_11

action_71 _ = happyReduce_12

action_72 (12) = happyShift action_6
action_72 (13) = happyShift action_7
action_72 (14) = happyShift action_8
action_72 (30) = happyShift action_9
action_72 (37) = happyShift action_10
action_72 (4) = happyGoto action_73
action_72 (5) = happyGoto action_2
action_72 (6) = happyGoto action_3
action_72 (7) = happyGoto action_4
action_72 (8) = happyGoto action_5
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (31) = happyShift action_74
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_13

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

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (EProd happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (EDiv happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happyReduce 7 8 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (ESeleccion happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 7 8 happyReduction_12
happyReduction_12 (_ `HappyStk`
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

happyReduce_13 = happyReduce 9 8 happyReduction_13
happyReduction_13 (_ `HappyStk`
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

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 (HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn8
		 (ERelacion happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 (HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyTerminal (TIdentifier happy_var_3))
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  10 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (PAnd happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (POr happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  10 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (PNot happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  10 happyReduction_21
happyReduction_21 (HappyTerminal (TIdentifier happy_var_3))
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn10
		 (PAttrEq happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  10 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn10
		 (PEq  happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  10 happyReduction_23
happyReduction_23 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn10
		 (PNeq happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  10 happyReduction_24
happyReduction_24 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn10
		 (PLt  happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  10 happyReduction_25
happyReduction_25 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn10
		 (PGt  happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  10 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn10
		 (PTrue
	)

happyReduce_27 = happySpecReduce_1  10 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn10
		 (PFalse
	)

happyReduce_28 = happySpecReduce_3  10 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  11 happyReduction_29
happyReduction_29 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn11
		 (VInt happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  11 happyReduction_30
happyReduction_30 (HappyTerminal (TString happy_var_1))
	 =  HappyAbsSyn11
		 (VString happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  11 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn11
		 (VBool True
	)

happyReduce_32 = happySpecReduce_1  11 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn11
		 (VBool False
	)

happyReduce_33 = happySpecReduce_1  11 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn11
		 (VNull
	)

happyNewToken action sts stk [] =
	action 40 40 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TSelect -> cont 12;
	TProject -> cont 13;
	TRename -> cont 14;
	TUnion -> cont 15;
	TDiferencia -> cont 16;
	TInterseccion -> cont 17;
	TProducto -> cont 18;
	TDivision -> cont 19;
	TNaturalJoin -> cont 20;
	TAnd -> cont 21;
	TOr -> cont 22;
	TNot -> cont 23;
	TTrue -> cont 24;
	TFalse -> cont 25;
	TEq -> cont 26;
	TNeq -> cont 27;
	TLt -> cont 28;
	TGt -> cont 29;
	TLParen -> cont 30;
	TRParen -> cont 31;
	TLBracket -> cont 32;
	TRBracket -> cont 33;
	TComma -> cont 34;
	TArrow -> cont 35;
	TNull -> cont 36;
	TIdentifier happy_dollar_dollar -> cont 37;
	TInt happy_dollar_dollar -> cont 38;
	TString happy_dollar_dollar -> cont 39;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 40 tk tks = happyError' (tks, explist)
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
parseExpr tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Either String a
parseError [] =
  Left "Error de sintaxis: fin inesperado de la entrada"

parseError (tok:_) =
  Left ("Error de sintaxis cerca de: " ++ show tok)
-------------------------------------------------------------
-- Lexer
-------------------------------------------------------------
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
--    | TSemicolon
    | TArrow
    | TNull
    | TIdentifier String
    | TInt Int
    | TString String
    deriving (Show, Eq)






lexer :: String -> Either Err [Token]
lexer [] = Right []
lexer (c:cs)

  | isSpace c = lexer cs

  | c == '('  = add TLParen
  | c == ')'  = add TRParen
  | c == '['  = add TLBracket
  | c == ']'  = add TRBracket
  | c == ','  = add TComma
--  | c == ';'  = add TSemicolon
  | c == '='  = add TEq
  | c == '<'  = add TLt
  | c == '>'  = add TGt

  | c == '-' && not (null cs) && head cs == '>'
    = prepend TArrow (tail cs)

  | c == '!' && not (null cs) && head cs == '='
      = prepend TNeq (tail cs)

  | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in prepend (TInt (read num)) rest

  | c == '"' =
      case span (/= '"') cs of
        (str, '"':rest) -> prepend (TString str) rest
        _ -> Left "String sin cerrar"

  | isAlpha c || c == '_' =
      let (word, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
      in prepend (keyword word) rest

  | otherwise = Left ("Caracter inesperado: " ++ [c])

  where
    add tok = prepend tok cs

    prepend tok rest =
      case lexer rest of
        Left err -> Left err
        Right ts -> Right (tok : ts)

keyword :: String -> Token
keyword w = case w of
    "seleccion"       -> TSelect
    "proyeccion"   -> TProject
    "renombre"       -> TRename
    "union"        -> TUnion
    "diferencia"   -> TDiferencia
    "interseccion" -> TInterseccion
    "producto"     -> TProducto
    "division"     -> TDivision
    "productoNatural"  -> TNaturalJoin
    "and"          -> TAnd
    "or"           -> TOr
    "not"          -> TNot
    "true"         -> TTrue
    "false"        -> TFalse
    "null"         -> TNull
    _              -> TIdentifier w



parse :: String -> Either String Expr
parse input = do
  toks <- lexer input
  parseExpr toks
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

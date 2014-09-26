{-# OPTIONS -Wall -Werror #-}

{-
 - 三値論理型Ternaryと関連関数
 - BoolとTernaryの連携
 - Boolの関数追加
 -}

module Ternary
( Ternary (..)
, toBool
, toTernary
, (&&&)
, (|||)
, (!&&)
, (!||)
, (^||)
, (^!|)
, (!&)
, (!|)
, (^|)
, (^!)
, not'
, and'
, or'
, nand
, nand'
, nor
, nor'
, xor
, xor'
, xnor
, xnor'
) where

-- 三値論理型Ternary
data Ternary = Unknown | True' | False' deriving (Eq)
instance Show Ternary where
	show Unknown = "Unknown"
	show True' = "True"
	show _ = "False"

-- TernaryからBool
toBool :: Ternary -> Maybe Bool
toBool True' = Just True
toBool False' = Just False
toBool _ = Nothing

-- BoolからTernary
toTernary :: Bool -> Ternary
toTernary True = True'
toTernary _ = False'

-- Ternaryの論理積
infixr 3 &&&
(&&&) :: Ternary -> Ternary -> Ternary
Unknown &&& _ = Unknown
_ &&& Unknown = Unknown
True' &&& x = x
False' &&& _ = False'

-- ﾎﾐリストの要素すべての論理積をとる
-- Unknownが有限個目にある場合、無限リストを扱える 
and' :: [Ternary] -> Ternary
and' = foldr (&&&) True'

-- Ternaryの論理和
infixr 2 |||
(|||) :: Ternary -> Ternary -> Ternary
Unknown ||| _ = Unknown
_ ||| Unknown = Unknown
False' ||| x = x
True' ||| _ = True'

-- Ternaryのリストの要素すべての論理和をとる
-- Unknownが有限個目にある場合、無限リストを扱える
or' :: [Ternary] -> Ternary
or' = foldr (|||) False'

-- Ternaryの論理反転
not' :: Ternary -> Ternary
not' True' = False'
not' False' = True'
not' _ = Unknown

-- TernaryのListの否定論理積
nand' :: [Ternary] -> Ternary
nand' = foldr (!&&) False'

-- TernaryのListの否定論理和
nor' :: [Ternary] -> Ternary
nor' = foldr (!||) True'

-- TernaryのListの排他的論理和
xor' :: [Ternary] -> Ternary
xor' = foldr (^||) True'

-- TernaryのListの一致
xnor' :: [Ternary] -> Ternary
xnor' = foldr (^!|) True'

-- BoolListの nand
nand :: [Bool] -> Bool
nand = foldr (!&) False

-- BoolListの nor
nor :: [Bool] -> Bool
nor = foldr (!|) True 

-- BoolListの xor
xor :: [Bool] -> Bool
xor = foldr (^|) True

-- BoolListの xnor
xnor :: [Bool] -> Bool
xnor = foldr (^!) True

-- Ternaryの否定論理積記号
infixr 3 !&&
(!&&) :: Ternary -> Ternary -> Ternary
x !&& y = not' $ x &&& y

-- Ternaryの否定論理和記号
infixr 2 !||
(!||) :: Ternary -> Ternary -> Ternary
x !|| y = not' $ x ||| y

-- Ternaryの排他的論理和記号
infixr 2 ^||
(^||) :: Ternary -> Ternary -> Ternary
x ^|| y = (x &&& y) &&& (x !|| y)

-- Ternaryの一致記号
infixr 2 ^!|
(^!|) :: Ternary -> Ternary -> Ternary
x ^!| y = not' $ x ^|| y

--Bool nand記号
infixr 3 !&
(!&) :: Bool -> Bool -> Bool
x !& y = not $ x && y

-- Bool nor記号
infixr 2 !|
(!|) :: Bool -> Bool -> Bool
x !| y = not $ x !| y

-- Bool xor記号
infixr 2 ^|
(^|) :: Bool -> Bool -> Bool
x ^| y = (x && y) && (x !| y)

-- Bool xnor記号
infixr 2 ^!
(^!) :: Bool -> Bool -> Bool
x ^! y = not $ x ^! y

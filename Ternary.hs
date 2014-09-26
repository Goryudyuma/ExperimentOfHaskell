{-# OPTIONS -Wall -Werror #-}

{-
 - �O�l�_���^Ternary�Ɗ֘A�֐�
 - Bool��Ternary�̘A�g
 - Bool�̊֐��ǉ�
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

-- �O�l�_���^Ternary
data Ternary = Unknown | True' | False' deriving (Eq)
instance Show Ternary where
	show Unknown = "Unknown"
	show True' = "True"
	show _ = "False"

-- Ternary����Bool
toBool :: Ternary -> Maybe Bool
toBool True' = Just True
toBool False' = Just False
toBool _ = Nothing

-- Bool����Ternary
toTernary :: Bool -> Ternary
toTernary True = True'
toTernary _ = False'

-- Ternary�̘_����
infixr 3 &&&
(&&&) :: Ternary -> Ternary -> Ternary
Unknown &&& _ = Unknown
_ &&& Unknown = Unknown
True' &&& x = x
False' &&& _ = False'

-- �Ѓ��X�g�̗v�f���ׂĂ̘_���ς��Ƃ�
-- Unknown���L���ڂɂ���ꍇ�A�������X�g�������� 
and' :: [Ternary] -> Ternary
and' = foldr (&&&) True'

-- Ternary�̘_���a
infixr 2 |||
(|||) :: Ternary -> Ternary -> Ternary
Unknown ||| _ = Unknown
_ ||| Unknown = Unknown
False' ||| x = x
True' ||| _ = True'

-- Ternary�̃��X�g�̗v�f���ׂĂ̘_���a���Ƃ�
-- Unknown���L���ڂɂ���ꍇ�A�������X�g��������
or' :: [Ternary] -> Ternary
or' = foldr (|||) False'

-- Ternary�̘_�����]
not' :: Ternary -> Ternary
not' True' = False'
not' False' = True'
not' _ = Unknown

-- Ternary��List�̔ے�_����
nand' :: [Ternary] -> Ternary
nand' = foldr (!&&) False'

-- Ternary��List�̔ے�_���a
nor' :: [Ternary] -> Ternary
nor' = foldr (!||) True'

-- Ternary��List�̔r���I�_���a
xor' :: [Ternary] -> Ternary
xor' = foldr (^||) True'

-- Ternary��List�̈�v
xnor' :: [Ternary] -> Ternary
xnor' = foldr (^!|) True'

-- BoolList�� nand
nand :: [Bool] -> Bool
nand = foldr (!&) False

-- BoolList�� nor
nor :: [Bool] -> Bool
nor = foldr (!|) True 

-- BoolList�� xor
xor :: [Bool] -> Bool
xor = foldr (^|) True

-- BoolList�� xnor
xnor :: [Bool] -> Bool
xnor = foldr (^!) True

-- Ternary�̔ے�_���ϋL��
infixr 3 !&&
(!&&) :: Ternary -> Ternary -> Ternary
x !&& y = not' $ x &&& y

-- Ternary�̔ے�_���a�L��
infixr 2 !||
(!||) :: Ternary -> Ternary -> Ternary
x !|| y = not' $ x ||| y

-- Ternary�̔r���I�_���a�L��
infixr 2 ^||
(^||) :: Ternary -> Ternary -> Ternary
x ^|| y = (x &&& y) &&& (x !|| y)

-- Ternary�̈�v�L��
infixr 2 ^!|
(^!|) :: Ternary -> Ternary -> Ternary
x ^!| y = not' $ x ^|| y

--Bool nand�L��
infixr 3 !&
(!&) :: Bool -> Bool -> Bool
x !& y = not $ x && y

-- Bool nor�L��
infixr 2 !|
(!|) :: Bool -> Bool -> Bool
x !| y = not $ x !| y

-- Bool xor�L��
infixr 2 ^|
(^|) :: Bool -> Bool -> Bool
x ^| y = (x && y) && (x !| y)

-- Bool xnor�L��
infixr 2 ^!
(^!) :: Bool -> Bool -> Bool
x ^! y = not $ x ^! y

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

-- Ternary�̔ے�_����
nand' :: Ternary -> Ternary -> Ternary
x `nand'` y = not' $ x &&& y

-- Ternary�̔ے�_���a
nor' :: Ternary -> Ternary -> Ternary
x `nor'` y = not' $ x ||| y

-- Ternary�̔r���I�_���a
xor' :: Ternary -> Ternary -> Ternary
x `xor'` y = (x &&& y) &&& (x `nor'` y)

-- Ternary�̈�v
xnor' :: Ternary -> Ternary -> Ternary
x `xnor'` y = not' $ x `xor'` y

-- Bool nand
nand :: Bool -> Bool -> Bool
x `nand` y = not $ x && y

-- Bool nor
nor :: Bool -> Bool -> Bool
x `nor` y = not $ x || y

-- Bool xor
xor :: Bool -> Bool -> Bool
x `xor` y = (x && y) && (x `nor` y)

-- Bool xnor
xnor :: Bool -> Bool -> Bool
x `xnor` y = not $ x `xor` y

-- Ternary�̔ے�_���ϋL��
(!&&) :: Ternary -> Ternary -> Ternary
x !&& y = x `nand'` y

-- Ternary�̔ے�_���a�L��
(!||) :: Ternary -> Ternary -> Ternary
x !|| y = x `nor'` y

-- Ternary�̔r���I�_���a�L��
(^||) :: Ternary -> Ternary -> Ternary
x ^|| y = x `xor'` y

-- Ternary�̈�v�L��
(^!|) :: Ternary -> Ternary -> Ternary
x ^!| y = x `xnor'` y

--Bool nand�L��
(!&) :: Bool -> Bool -> Bool
x !& y = x `nand` y

-- Bool nor�L��
(!|) :: Bool -> Bool -> Bool
x !| y = x `nor` y

-- Bool xor�L��
(^|) :: Bool -> Bool -> Bool
x ^| y = x `xor` y

-- Bool xnor�L��
(^!) :: Bool -> Bool -> Bool
x ^! y = x `xnor` y
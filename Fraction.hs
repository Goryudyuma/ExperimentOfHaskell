{-# OPTIONS -Wall -Werror #-}

{- 
 - �ѕ������������W���[��Fraction
 - Fraction�𒼐ڍ쐬�͂ł��܂���B
 - /- ���Z�q�Ȃǂ��g���Ă��������B
 -  -}

module Fraction
( Fraction
, (/-)
, (/-:+)
, (/-:-)
, (/-:*)
, (/-:/)
, reduce
, toDouble
, toImproper
, assign'
) where

type Numerator = Int
type Denominator = Int


-- �ѕ���
data Fraction = Invalid | Fraction Int (Numerator, Denominator) 
 deriving (Show, Read, Eq, Ord)

-- �ѕ��������߂�֐�
infixl 9 /-
(/-) :: Int -> (Int, Int) -> Fraction
x /- (y,z)
	| z == 0 = Invalid
	| z < 0 = x /- (negate y, abs z)
	| y < 0 = (x - 1) /- (y + z, z)
	| y < z = Fraction x (y,z)
	| otherwise = (x + 1) /- (y-z, z)

-- Fraction���m�̑����Z
infixr 5 /-:+
(/-:+) :: Fraction -> Fraction -> Fraction
<<<<<<< HEAD
(Fraction a (b,c)) /-:+ (Fraction x (y,z)) = reduce $ (a+x) /- (b*z + c*y, c*z)
_ /-:+ _ = Invalid
=======
Invalid /-:+ _ = Invalid
_ /-:+ Invalid = Invalid
(Fraction a (b,c)) /-:+ (Fraction x (y,z)) = reduce ((a+x) /- (b*z + c*y, c*z))
>>>>>>> parent of c78fbb4... 関数のパターンマッチの簡略化

-- Fraction���m�̈����Z
infixr 5 /-:-
(/-:-) :: Fraction -> Fraction -> Fraction
<<<<<<< HEAD
(Fraction a (b,c)) /-:- (Fraction x (y,z)) = reduce $ (a-x) /- (b*z - c*y, c*z)
_ /-:- _ = Invalid
=======
Invalid /-:- _ = Invalid
_ /-:- Invalid = Invalid
(Fraction a (b,c)) /-:- (Fraction x (y,z)) = reduce ((a-x) /- (b*z - c*y, c*z))
>>>>>>> parent of c78fbb4... 関数のパターンマッチの簡略化

-- Fraction���m�̊|���Z
infixr 6 /-:*
(/-:*) :: Fraction -> Fraction -> Fraction
Invalid /-:* _ = Invalid
_ /-:* Invalid = Invalid
(Fraction a (b,c)) /-:* (Fraction x (y,z))
	| a == 0 && x == 0 = reduce (0 /- (b*y, c*z))
	| a /= 0 = toImproper (Fraction a (b, c)) /-:* (Fraction x (y, z))
	| otherwise = (Fraction a (b, c)) /-:* (toImproper $ Fraction x (y, z))

-- Fraction���m�̊���Z
infixr 6 /-:/
(/-:/) :: Fraction -> Fraction -> Fraction
<<<<<<< HEAD
(Fraction a (b,c)) /-:/ (Fraction x (y,z)) = (Fraction a (b,c)) /-:* toReverse $ Fraction x (y,z)
_ /-:/ _ = Invalid
=======
Invalid /-:/ _ = Invalid
_ /-:/ Invalid = Invalid
(Fraction a (b,c)) /-:/ (Fraction x (y,z)) = (Fraction a (b,c)) /-:* toReverse (Fraction x (y,z))
>>>>>>> parent of c78fbb4... 関数のパターンマッチの簡略化

-- �񕪂���֐�
reduce :: Fraction -> Fraction
reduce Invalid = Invalid
reduce (Fraction a (b,c)) = a /- (b `div` (gcd b c), c `div` (gcd b c))

-- Double�ɕϊ�����֐�
toDouble :: Fraction -> Maybe Double
toDouble Invalid = Nothing
toDouble (Fraction x (y, z)) = Just $ fromIntegral (x*z + y) / fromIntegral z

--������(improper fractions)�ɂ���֐�
toImproper :: Fraction -> Fraction
toImproper Invalid = Invalid
toImproper (Fraction x (y, z)) = Fraction 0 (x*z+y, z)

--y��z���Ђ�����Ԃ��֐��B
toReverse :: Fraction -> Fraction
toReverse Invalid = Invalid
toReverse (Fraction x (y, z)) 
	| x == 0 = Fraction x (z, y)
	| otherwise = toReverse $ toImproper $ Fraction x (y, z)
<<<<<<< HEAD
toReverse _ = Invalid

--�����������w�肵�Čv�Z���ʂ�Ԃ��֐��B
assign' :: Int -> Fraction -> Fraction
assign' a (Fraction x (y,z)) = Fraction a ((x-a)*z+y,z)
assign' _ _ = Invalid


=======
>>>>>>> parent of c78fbb4... 関数のパターンマッチの簡略化

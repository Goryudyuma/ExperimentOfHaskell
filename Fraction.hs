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
) where

type Numerator = Int
type Denominator = Int


-- �ѕ���
data Fraction = Fraction Int (Numerator, Denominator)
 deriving (Show, Read, Eq, Ord)

-- �ѕ��������߂�֐�
infixl 9 /-
(/-) :: Int -> (Int, Int) -> Fraction
x /- (y,z)
	| z < 0 = x /- (-y, -z)
	| z == 0 = error "Denominator of the fraction is 0"
	| y < 0 = (x - 1) /- (y + z, z)
	| y < z = Fraction x (y,z)
	| otherwise = (x + 1) /- (y-z, z)

-- Fraction���m�̑����Z
infixr 5 /-:+
(/-:+) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:+ (Fraction x (y,z)) = reduce ((a+x) /- (b*z + c*y, c*z))

-- Fraction���m�̈����Z
infixr 5 /-:-
(/-:-) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:- (Fraction x (y,z)) = reduce ((a-x) /- (b*z - c*y, c*z))

-- Fraction���m�̊|���Z
infixr 6 /-:*
(/-:*) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))

-- Fraction���m�̊���Z
infixr 6 /-:/
(/-:/) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:/ (Fraction x (y,z)) = (Fraction a (b,c)) /-:* toReverse(toImproper(Fraction x (y,z)))

-- �񕪂���֐�
reduce :: Fraction -> Fraction
reduce (Fraction a (b,c)) = a /-(b `div` (gcd b c), c `div` (gcd b c))

-- Double�ɕϊ�����֐�
toDouble :: Fraction -> Double
toDouble (Fraction x (y, z)) = fromIntegral (x*z + y) / fromIntegral z

--������(improper fractions)�ɂ���֐�
toImproper :: Fraction -> Fraction
toImproper (Fraction x (y, z)) = (Fraction 0 (x*z+y, z))

--y��z���Ђ�����Ԃ��֐��B
toReverse :: Fraction -> Fraction
toReverse (Fraction x (y, z)) 
	| x == 0 = Fraction x (z, y)
	| otherwise = toImproper (Fraction x (y, z))

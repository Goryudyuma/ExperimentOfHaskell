{-# OPTIONS -Wall -Werror #-}

{- 
 - 帯分数を扱うモジュールFraction
 - Fractionを直接作成はできません。
 - /- 演算子などを使ってください。
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


-- 帯分数
data Fraction = Fraction Int (Numerator, Denominator)
 deriving (Show, Read, Eq, Ord)

-- 帯分数を求める関数
infixl 9 /-
(/-) :: Int -> (Int, Int) -> Fraction
x /- (y,z)
	| z < 0 = x /- (-y, -z)
	| z == 0 = error "Denominator of the fraction is 0"
	| y < 0 = (x - 1) /- (y + z, z)
	| y < z = Fraction x (y,z)
	| otherwise = (x + 1) /- (y-z, z)

-- Fraction同士の足し算
infixr 5 /-:+
(/-:+) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:+ (Fraction x (y,z)) = reduce ((a+x) /- (b*z + c*y, c*z))

-- Fraction同士の引き算
infixr 5 /-:-
(/-:-) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:- (Fraction x (y,z)) = reduce ((a-x) /- (b*z - c*y, c*z))

-- Fraction同士の掛け算
infixr 6 /-:*
(/-:*) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))

-- Fraction同士の割り算
infixr 6 /-:/
(/-:/) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:/ (Fraction x (y,z)) = (Fraction a (b,c)) /-:* toReverse(toImproper(Fraction x (y,z)))

-- 約分する関数
reduce :: Fraction -> Fraction
reduce (Fraction a (b,c)) = a /-(b `div` (gcd b c), c `div` (gcd b c))

-- Doubleに変換する関数
toDouble :: Fraction -> Double
toDouble (Fraction x (y, z)) = fromIntegral (x*z + y) / fromIntegral z

--仮分数(improper fractions)にする関数
toImproper :: Fraction -> Fraction
toImproper (Fraction x (y, z)) = (Fraction 0 (x*z+y, z))

--yとzをひっくり返す関数。
toReverse :: Fraction -> Fraction
toReverse (Fraction x (y, z)) 
	| x == 0 = Fraction x (z, y)
	| otherwise = toImproper (Fraction x (y, z))

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
, assign'
) where

type Numerator = Int
type Denominator = Int


-- 帯分数
data Fraction = Invalid | Fraction Int (Numerator, Denominator) 
 deriving (Show, Read, Eq, Ord)

-- 帯分数を求める関数
infixl 9 /-
(/-) :: Int -> (Int, Int) -> Fraction
x /- (y,z)
	| z == 0 = Invalid
	| z < 0 = x /- (negate y, abs z)
	| y < 0 = (x - 1) /- (y + z, z)
	| y < z = Fraction x (y,z)
	| otherwise = (x + 1) /- (y-z, z)

-- Fraction同士の足し算
infixr 5 /-:+
(/-:+) :: Fraction -> Fraction -> Fraction
<<<<<<< HEAD
(Fraction a (b,c)) /-:+ (Fraction x (y,z)) = reduce $ (a+x) /- (b*z + c*y, c*z)
_ /-:+ _ = Invalid
=======
Invalid /-:+ _ = Invalid
_ /-:+ Invalid = Invalid
(Fraction a (b,c)) /-:+ (Fraction x (y,z)) = reduce ((a+x) /- (b*z + c*y, c*z))
>>>>>>> parent of c78fbb4... 髢｢謨ｰ縺ｮ繝代ち繝ｼ繝ｳ繝槭ャ繝√�ｮ邁｡逡･蛹�

-- Fraction同士の引き算
infixr 5 /-:-
(/-:-) :: Fraction -> Fraction -> Fraction
<<<<<<< HEAD
(Fraction a (b,c)) /-:- (Fraction x (y,z)) = reduce $ (a-x) /- (b*z - c*y, c*z)
_ /-:- _ = Invalid
=======
Invalid /-:- _ = Invalid
_ /-:- Invalid = Invalid
(Fraction a (b,c)) /-:- (Fraction x (y,z)) = reduce ((a-x) /- (b*z - c*y, c*z))
>>>>>>> parent of c78fbb4... 髢｢謨ｰ縺ｮ繝代ち繝ｼ繝ｳ繝槭ャ繝√�ｮ邁｡逡･蛹�

-- Fraction同士の掛け算
infixr 6 /-:*
(/-:*) :: Fraction -> Fraction -> Fraction
Invalid /-:* _ = Invalid
_ /-:* Invalid = Invalid
(Fraction a (b,c)) /-:* (Fraction x (y,z))
	| a == 0 && x == 0 = reduce (0 /- (b*y, c*z))
	| a /= 0 = toImproper (Fraction a (b, c)) /-:* (Fraction x (y, z))
	| otherwise = (Fraction a (b, c)) /-:* (toImproper $ Fraction x (y, z))

-- Fraction同士の割り算
infixr 6 /-:/
(/-:/) :: Fraction -> Fraction -> Fraction
<<<<<<< HEAD
(Fraction a (b,c)) /-:/ (Fraction x (y,z)) = (Fraction a (b,c)) /-:* toReverse $ Fraction x (y,z)
_ /-:/ _ = Invalid
=======
Invalid /-:/ _ = Invalid
_ /-:/ Invalid = Invalid
(Fraction a (b,c)) /-:/ (Fraction x (y,z)) = (Fraction a (b,c)) /-:* toReverse (Fraction x (y,z))
>>>>>>> parent of c78fbb4... 髢｢謨ｰ縺ｮ繝代ち繝ｼ繝ｳ繝槭ャ繝√�ｮ邁｡逡･蛹�

-- 約分する関数
reduce :: Fraction -> Fraction
reduce Invalid = Invalid
reduce (Fraction a (b,c)) = a /- (b `div` (gcd b c), c `div` (gcd b c))

-- Doubleに変換する関数
toDouble :: Fraction -> Maybe Double
toDouble Invalid = Nothing
toDouble (Fraction x (y, z)) = Just $ fromIntegral (x*z + y) / fromIntegral z

--仮分数(improper fractions)にする関数
toImproper :: Fraction -> Fraction
toImproper Invalid = Invalid
toImproper (Fraction x (y, z)) = Fraction 0 (x*z+y, z)

--yとzをひっくり返す関数。
toReverse :: Fraction -> Fraction
toReverse Invalid = Invalid
toReverse (Fraction x (y, z)) 
	| x == 0 = Fraction x (z, y)
	| otherwise = toReverse $ toImproper $ Fraction x (y, z)
<<<<<<< HEAD
toReverse _ = Invalid

--整数部分を指定して計算結果を返す関数。
assign' :: Int -> Fraction -> Fraction
assign' a (Fraction x (y,z)) = Fraction a ((x-a)*z+y,z)
assign' _ _ = Invalid


=======
>>>>>>> parent of c78fbb4... 髢｢謨ｰ縺ｮ繝代ち繝ｼ繝ｳ繝槭ャ繝√�ｮ邁｡逡･蛹�

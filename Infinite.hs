{-# OPTIONS -Wall -Werror #-}

module Infinite
	( Infinite
	, infinite
	, lookInInfinite
	, getAsDouble
	, getAsDouble'
	, showWithPrecision
	) where

import Data.Monoid

-- Infiniteモジュール
--
-- Infinite : 無限精度実数型
--
infinite :: Double -> Infinite
-- infinite : Double型の値を渡すとInfinite型にして返す関数
--
getAsDouble :: Infinite -> Double
-- getAsDouble : 引数として受け取ったInfinite型の数をDouble型にして返す関数
--
getAsDouble' :: Int -> Infinite -> Double
-- getAsDouble' : 第１引数はgetAsDoubleに比べて何倍正確に変換するかをInt型で、第２引数は変換したいInfinite型の数
-- getAsDoubleでは正確な値が得られないときにご利用ください
--
showWithPrecision :: Int -> Infinite -> String
-- showWithPrecision : 小数点以下桁数を指定して文字列化
--
-- lookInInfinite : 引数として受け取ったInfinite型の内部実装を覗きたい物好きな人向けの関数
--





--------------------------------------------------------------------------------------





newtype Infinite = Infinite { lookInInfinite :: (Bool, Integer, [Bool]) }
	deriving (Eq)

instance Ord Infinite where
	Infinite (c, a, bs) `compare` Infinite (z, x, ys) = compare (if c then a else negate a) (if z then x else negate x) `mappend` fcmp bs ys
		where
		d `cmp` e = if c then d `compare` e else not d `compare` not e
		fcmp (s : ss) (t : ts) = cmp s t `mappend` fcmp ss ts
		fcmp [] (t : ts) = cmp False t `mappend` fcmp [] ts
		fcmp (s : ss) [] = cmp s False `mappend` fcmp ss []
		fcmp _ _ = EQ

instance Show Infinite where
	show d@(Infinite (_, _, bs)) = showWithPrecision (length bs) d 
{-
instance Num Infinite where
	abs (Infinite (_, a, bs)) = Infinite (True, a, bs)
	negate (Infinite (c, a, bs)) = Infinite (not c, a, bs)
	signum (Infinite (_, 0, [])) = Infinite (True, 0, [])
	signum (Infinite (c, _, _)) = Infinite (c, 0, [])
	fromInteger a = Infinite (a >= 0, a, [])
	Infinite (c, a, bs) + Infinite (z, x, ys) = 
-}
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Monoid (DiffList a) where
	mempty = DiffList ([] ++)
	DiffList f `mappend` DiffList g = DiffList (f . g)

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList = flip getDiffList []

infinite a = Infinite (a >= 0.0, intpart, fromDiffList . fracDiffList $ abs a - fromIntegral intpart)
	where
	intpart = floor . abs $ a

fracDiffList :: Double -> DiffList Bool
fracDiffList x
	| x == 0.0 = mempty
	| otherwise = toDiffList [floor (double) == (1 :: Int)] `mappend` fracDiffList (double - if double >= 1.0 then 1.0 else 0.0)
		where
		double = 2.0 * x 

unbinarize :: Int -> Infinite -> Double
unbinarize n (Infinite (c, a, bs)) = if c then fromIntegral a + dec n bs else fromIntegral a - dec n bs

dec :: Int -> [Bool] -> Double
dec n = fst . foldl (\(acc, ys) x -> (acc + if x then 1.0 / (2.0 ** head ys) else 0, tail ys)) (0.0, [1 .. ]) . take n

getAsDouble = unbinarize 100 

getAsDouble' n = unbinarize (100 * n)

showWithPrecision n (Infinite (c, a, bs)) = (if c then "" else "-") ++ show a ++ (take (n + 1) . tail . show . dec (4 * (n + 1))) bs

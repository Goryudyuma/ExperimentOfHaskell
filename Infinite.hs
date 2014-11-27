{-# OPTIONS -Wall -Werror #-}

module Infinite
	( Infinite
	, infinite
	, lookInInfinite
	, getAsDouble
	, getAsDouble'
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
-- lookInInfinite : 引数として受け取ったInfinite型の内部実装を覗きたい物好きな人向けの関数
--





--------------------------------------------------------------------------------------





newtype Infinite = Infinite { lookInInfinite :: (Integer, [Bool]) }
	deriving (Eq)

instance Ord Infinite where
	Infinite (a, bs) `compare` Infinite (x, ys) = compare a x `mappend` fcmp bs ys
		where
		c `cmp` d = if a < 0 then not c `compare` not d else c `compare` d
		fcmp (s : ss) (t : ts) = cmp s t `mappend` fcmp ss ts
		fcmp [] (t : ts) = cmp False t `mappend` fcmp [] ts
		fcmp (s : ss) [] = cmp s False `mappend` fcmp ss []
		fcmp _ _ = EQ

instance Show Infinite where
	show (Infinite (a, bs)) = show a ++ (tail . show . dec (length bs)) bs

{-
instance Num Infinite where
	abs (Infinite (a, bs)) = Infinite (abs a, bs)
	negate (Infinite (a, bs)) = Infinite (negate a, bs)
	signum (Infinite (a, bs)) = signum a
	fromInteger a = Infinite (a, [])
	Infinite (a, bs) + Infinite (x, ys) = 
-}
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Monoid (DiffList a) where
	mempty = DiffList ([] ++)
	DiffList f `mappend` DiffList g = DiffList (f . g)

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList = flip getDiffList []

sgn :: (Num a, Ord a, Num b) => a -> b
sgn x
	| x >= 0 = 1
	| otherwise = (-1)


infinite a = Infinite (sgn a * intpart, fromDiffList . fracDiffList $ abs a - fromIntegral intpart)
	where
	intpart = floor . abs $ a

fracDiffList :: Double -> DiffList Bool
fracDiffList x
	| x == 0.0 = mempty
	| otherwise = toDiffList [floor (double) == (1 :: Int)] `mappend` fracDiffList (double - if double >= 1.0 then 1.0 else 0.0)
		where
		double = 2.0 * x 

unbinarize :: Int -> Infinite -> Double
unbinarize n (Infinite (a, bs)) = fromIntegral a + sgn a * dec n bs

dec :: Int -> [Bool] -> Double
dec n = fst . foldl (\(acc, ys) x -> (acc + if x then 1.0 / (2.0 ** head ys) else 0, tail ys)) (0.0, [1 .. ]) . take n

getAsDouble = unbinarize 100 

getAsDouble' n = unbinarize (100 * n)


{-# OPTIONS -Wall -Werror #-}

module Infinite
	( Infinite
	, infinite
	, lookInInfinite
	, getAsDouble
	, getAsDouble'
	) where

import Data.Monoid

-- Infinite���W���[��
--
-- Infinite : �������x�����^
--
infinite :: Double -> Infinite
-- infinite : Double�^�̒l��n����Infinite�^�ɂ��ĕԂ��֐�
--
getAsDouble :: Infinite -> Double
-- getAsDouble : �����Ƃ��Ď󂯎����Infinite�^�̐���Double�^�ɂ��ĕԂ��֐�
--
getAsDouble' :: Int -> Infinite -> Double
-- getAsDouble' : ��P������getAsDouble�ɔ�ׂĉ��{���m�ɕϊ����邩��Int�^�ŁA��Q�����͕ϊ�������Infinite�^�̐�
-- getAsDouble�ł͐��m�Ȓl�������Ȃ��Ƃ��ɂ����p��������
--
-- lookInInfinite : �����Ƃ��Ď󂯎����Infinite�^�̓���������`���������D���Ȑl�����̊֐�
--





--------------------------------------------------------------------------------------





newtype Infinite = Infinite { lookInInfinite :: (Integer, [Bool]) }
	deriving (Eq)

instance Ord Infinite where
	Infinite (a, bs) `compare` Infinite (x, ys) = compare a x `mappend` fcmp bs ys
		where
		c `cmp` d = if a < 0 then not c `compare` not d else c `compare` d
		fcmp (s : ss) (t : ts) = cmp s t `mappend` fcmp ss ts
		fcmp _ _ = EQ

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
unbinarize n (Infinite (a, bs)) = fromIntegral a + sgn a * (fst . foldl (\(acc, ys) x -> (acc + if x then 1.0 / (2.0 ** head ys) else 0, tail ys)) (0.0, [1..]) . take n) bs

getAsDouble = unbinarize 100 

getAsDouble' n = unbinarize (100 * n)


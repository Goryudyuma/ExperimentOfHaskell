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

-- Infiniteƒ‚ƒWƒ…[ƒ‹
--
-- Infinite : –³ŒÀ¸“xŽÀ”Œ^
--
infinite :: Double -> Infinite
-- infinite : DoubleŒ^‚Ì’l‚ð“n‚·‚ÆInfiniteŒ^‚É‚µ‚Ä•Ô‚·ŠÖ”
--
getAsDouble :: Infinite -> Double
-- getAsDouble : ˆø”‚Æ‚µ‚ÄŽó‚¯Žæ‚Á‚½InfiniteŒ^‚Ì”‚ðDoubleŒ^‚É‚µ‚Ä•Ô‚·ŠÖ”
--
getAsDouble' :: Int -> Infinite -> Double
-- getAsDouble' : ‘æ‚Pˆø”‚ÍgetAsDouble‚É”ä‚×‚Ä‰½”{³Šm‚É•ÏŠ·‚·‚é‚©‚ðIntŒ^‚ÅA‘æ‚Qˆø”‚Í•ÏŠ·‚µ‚½‚¢InfiniteŒ^‚Ì”
-- getAsDouble‚Å‚Í³Šm‚È’l‚ª“¾‚ç‚ê‚È‚¢‚Æ‚«‚É‚²—˜—p‚­‚¾‚³‚¢
--
showWithPrecision :: Int -> Infinite -> String
-- showWithPrecision : ¬”“_ˆÈ‰ºŒ…”‚ðŽw’è‚µ‚Ä•¶Žš—ñ‰»
--
-- lookInInfinite : ˆø”‚Æ‚µ‚ÄŽó‚¯Žæ‚Á‚½InfiniteŒ^‚Ì“à•”ŽÀ‘•‚ð”`‚«‚½‚¢•¨D‚«‚ÈlŒü‚¯‚ÌŠÖ”
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

showWithPrecision n (Infinite (c, a, bs)) = if c then "" else "-" ++ show a ++ (take (n + 1) . tail . show . dec (4 * (n + 1))) bs

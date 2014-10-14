{-# OPTIONS -Wall -Werror #-}

module ExtendedFractions
( InfinitePrecision
, (.?)
) where 


data InfinitePrecision = InfinitePrecision Integer Integer
	deriving (Eq, Ord)

instance Show InfinitePrecision where
	show (InfinitePrecision x y) = show x ++ "." ++ show y


infixl 8 .?
(.?) :: (Integral a) => a -> a -> InfinitePrecision
x .? y
	| y < 0 = negate x .? abs y
	| otherwise = InfinitePrecision (fromIntegral x) (fromIntegral y)

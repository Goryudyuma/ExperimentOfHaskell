{-# OPTIONS -Wall -Werror #-}

-- ‘Ñ•ª”
data Fraction = Fraction Int (Int, Int)
 deriving (Show, Eq, Ord)

-- ‘Ñ•ª”‚ð‹‚ß‚éŠÖ”
infixl 9 /-
(/-) :: Int -> (Int, Int) -> Fraction
x /- (y,z)
	| y < z = Fraction x (y,z)
	| y < 0 = (x - 1) /- (y + z, z)
	| otherwise = (x + 1) /- (y-z, z)

-- Fraction“¯Žm‚Ì‘«‚µŽZ
infixr 5 /-:+
(/-:+) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:+ (Fraction x (y,z)) = reduce ((a+x) /- (b*z + c*y, c*z))

-- Fraction“¯Žm‚Ìˆø‚«ŽZ
infixr 5 /-:-
(/-:-) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:- (Fraction x (y,z)) = reduce ((a-x) /- (b*z - c*y, c*z))

-- Fraction“¯Žm‚ÌŠ|‚¯ŽZ
infixr 6 /-:*
(/-:*) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))


-- –ñ•ª‚·‚éŠÖ”
reduce :: Fraction -> Fraction
reduce (Fraction a (b,c)) = Fraction a (b `div` (gcd b c), c `div` (gcd b c))

-- Double‚É•ÏŠ·‚·‚éŠÖ”
toDouble :: Fraction -> Double
toDouble (Fraction x (y, z)) = fromIntegral (x*z + y) / fromIntegral z

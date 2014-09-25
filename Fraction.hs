{-# OPTIONS -Wall -Werror #-}

{- 
 - ‘Ñ•ª”‚ðˆµ‚¤ƒ‚ƒWƒ…[ƒ‹Fraction
 - Fraction‚ð’¼Úì¬‚Í‚Å‚«‚Ü‚¹‚ñB
 - /- ‰‰ŽZŽq‚È‚Ç‚ðŽg‚Á‚Ä‚­‚¾‚³‚¢B
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


-- ‘Ñ•ª”
data Fraction = Invalid | Fraction Int (Numerator, Denominator)
 deriving (Show, Read, Eq, Ord)

-- ‘Ñ•ª”‚ð‹‚ß‚éŠÖ”
infixl 9 /-
(/-) :: Int -> (Int, Int) -> Fraction
x /- (y,z)
	| z == 0 = Invalid
	| z < 0 = x /- (negate y, abs z)
	| y < 0 = (x - 1) /- (y + z, z)
	| y < z = Fraction x (y,z)
	| otherwise = (x + 1) /- (y-z, z)

-- Fraction“¯Žm‚Ì‘«‚µŽZ
infixr 5 /-:+
(/-:+) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:+ (Fraction x (y,z)) = reduce ((a+x) /- (b*z + c*y, c*z))
_ /-:+ _ = Invalid

-- Fraction“¯Žm‚Ìˆø‚«ŽZ
infixr 5 /-:-
(/-:-) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:- (Fraction x (y,z)) = reduce ((a-x) /- (b*z - c*y, c*z))
_ /-:- _ = Invalid

-- Fraction“¯Žm‚ÌŠ|‚¯ŽZ
infixr 6 /-:*
(/-:*) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:* (Fraction x (y,z))
	| a == 0 && x == 0 = reduce (0 /- (b*y, c*z))
	| a /= 0 = toImproper (Fraction a (b, c)) /-:* (Fraction x (y, z))
	| otherwise = (Fraction a (b, c)) /-:* (toImproper $ Fraction x (y, z))
_ /-:* _ = Invalid

-- Fraction“¯Žm‚ÌŠ„‚èŽZ
infixr 6 /-:/
(/-:/) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:/ (Fraction x (y,z)) = (Fraction a (b,c)) /-:* toReverse (Fraction x (y,z))
_ /-:/ _ = Invalid

-- –ñ•ª‚·‚éŠÖ”
reduce :: Fraction -> Fraction
reduce (Fraction a (b,c)) = a /- (b `div` (gcd b c), c `div` (gcd b c))
reduce _ = Invalid

-- Double‚É•ÏŠ·‚·‚éŠÖ”
toDouble :: Fraction -> Maybe Double
toDouble (Fraction x (y, z)) = Just $ fromIntegral (x*z + y) / fromIntegral z
toDouble _ = Nothing

--‰¼•ª”(improper fractions)‚É‚·‚éŠÖ”
toImproper :: Fraction -> Fraction
toImproper (Fraction x (y, z)) = Fraction 0 (x*z+y, z)
toImproper _ = Invalid

--y‚Æz‚ð‚Ð‚Á‚­‚è•Ô‚·ŠÖ”B
toReverse :: Fraction -> Fraction
toReverse (Fraction x (y, z)) 
	| x == 0 = Fraction x (z, y)
	| otherwise = toReverse $ toImproper $ Fraction x (y, z)
toReverse _ = Invalid


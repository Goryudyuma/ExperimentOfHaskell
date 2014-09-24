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

<<<<<<< HEAD
-- Fraction Int (Int, Int)“¯Žm‚ÌŠ|‚¯ŽZ
<<<<<<< HEAD
infixr 5 ++*
(++*) :: Fraction Int (Int, Int) -> Fraction Int (Int, Int) -> Fraction Int (Int, Int)
(Fraction a (b,c)) ++* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))
=======
=======
-- Fraction“¯Žm‚ÌŠ|‚¯ŽZ
>>>>>>> 2aaab91815bec166d1dcf8689bb5e4ac2ef73957
infixr 6 /-:*
(/-:*) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))
>>>>>>> 104a7ddc03c44e066957bf1d4bf00c2c77bcebeb


-- –ñ•ª‚·‚éŠÖ”
reduce :: Fraction -> Fraction
reduce (Fraction a (b,c)) = Fraction a (b `div` (gcd b c), c `div` (gcd b c))

{-# OPTIONS -Wall -Werror #-}

-- ‘Ñ•ª”
data Fraction a = Fraction a (a, a)
 deriving (Show, Read, Eq, Ord)

-- ‘Ñ•ª”‚ð‹‚ß‚éŠÖ”
infixl 9 /-
(/-) :: Int -> (Int, Int) -> Fraction Int
x /- (y,z)
	| y < z = Fraction x (y,z)
	| y < 0 = (x - 1) /- (y + z, z)
	| otherwise = (x + 1) /- (y-z, z)

-- Fraction Int (Int, Int)“¯Žm‚Ì‘«‚µŽZ
infixr 5 /-:+
(/-:+) :: Fraction Int -> Fraction Int -> Fraction Int
(Fraction a (b,c)) /-:+ (Fraction x (y,z)) = reduce ((a+x) /- (b*z + c*y, c*z))

-- Fraction Int (Int, Int)“¯Žm‚Ìˆø‚«ŽZ
infixr 5 /-:-
(/-:-) :: Fraction Int -> Fraction Int -> Fraction Int
(Fraction a (b,c)) /-:- (Fraction x (y,z)) = reduce ((a-x) /- (b*z - c*y, c*z))

-- Fraction Int (Int, Int)“¯Žm‚ÌŠ|‚¯ŽZ
<<<<<<< HEAD
infixr 5 ++*
(++*) :: Fraction Int (Int, Int) -> Fraction Int (Int, Int) -> Fraction Int (Int, Int)
(Fraction a (b,c)) ++* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))
=======
infixr 6 /-:*
(/-:*) :: Fraction Int -> Fraction Int -> Fraction Int
(Fraction a (b,c)) /-:* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))
>>>>>>> 104a7ddc03c44e066957bf1d4bf00c2c77bcebeb


-- –ñ•ª‚·‚éŠÖ”
reduce :: Fraction Int -> Fraction Int
reduce (Fraction a (b,c)) = Fraction a (b `div` (gcd b c), c `div` (gcd b c))

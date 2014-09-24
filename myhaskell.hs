{-# OPTIONS -Wall -Werror #-}

-- ‘Ñ•ª”
data Fraction a b = Fraction a b
 deriving (Show, Read, Eq, Ord)

-- ‘Ñ•ª”‚ð‹‚ß‚éŠÖ”
infixl 9 /-
(/-) :: Int -> (Int, Int) -> Fraction Int (Int,Int)
x /- (y,z)
 | y < z = Fraction x (y,z)
 | y < 0 = (x - 1) /- (y + z, z)
 | otherwise = (x + 1) /- (y-z, z)

-- Fraction Int (Int, Int)“¯Žm‚Ì‘«‚µŽZ
infixr 5 +++
(+++) :: Fraction Int (Int, Int) -> Fraction Int (Int, Int) -> Fraction Int (Int, Int)
(Fraction a (b,c)) +++ (Fraction x (y,z)) = reduce ((a+x) /- (b*z + c*y, c*z))

-- Fraction Int (Int, Int)“¯Žm‚Ìˆø‚«ŽZ
infixr 5 ++-
(++-) :: Fraction Int (Int, Int) -> Fraction Int (Int, Int) -> Fraction Int (Int, Int)
(Fraction a (b,c)) ++- (Fraction x (y,z)) = reduce ((a-x) /- (b*z - c*y, c*z))

-- Fraction Int (Int, Int)“¯Žm‚ÌŠ|‚¯ŽZ
infixr 5 ++*
(++*) :: Fraction Int (Int, Int) -> Fraction Int (Int, Int) -> Fraction Int (Int, Int)
(Fraction a (b,c)) ++* (Fraction x (y,z)) = reduce ((a*x) /- (b*y, c*z))


-- –ñ•ª‚·‚éŠÖ”
reduce :: Fraction Int (Int, Int) -> Fraction Int (Int, Int)
reduce (Fraction a (b,c)) = Fraction a (b `div` (gcd b c), c `div` (gcd b c))

{-# OPTIONS -Wall -Werror #-}

-- ŅĒ
data Fraction a b = Fraction a b
 deriving (Show, Read, Eq, Ord)

-- ŅĒđßéÖ
infixl 9 /-
(/-) :: Int -> (Int, Int) -> Fraction Int (Int,Int)
x /- (y,z)
 | y < z = Fraction x (y,z)
 | y < 0 = (x - 1) /- (y + z, z)
 | otherwise = (x + 1) /- (y-z, z)

-- Fraction Int (Int, Int)¯mĖĢĩZ
infixr 5 +++
(+++) :: Fraction Int (Int, Int) -> Fraction Int (Int, Int) -> Fraction Int (Int, Int)
(Fraction a (b,c)) +++ (Fraction x (y,z)) = reduce ((a+x) /- (b*z + c*y, c*z))

-- Fraction Int (Int, Int)¯mĖøĢZ
infixr 5 ++-
(++-) :: Fraction Int (Int, Int) -> Fraction Int (Int, Int) -> Fraction Int (Int, Int)
(Fraction a (b,c)) ++- (Fraction x (y,z)) = reduce ((a-x) /- (b*z - c*y, c*z))

-- Fraction Int (Int, Int)¯mĖ|¯Z
<<<<<<< HEAD
<<<<<<< HEAD
infixr 6 /-:*
(/-:*) :: Fraction Int (Int, Int) -> Fraction Int (Int, Int) -> Fraction Int (Int, Int)
(Fraction a (b,c)) /-:* (Fraction x (y,z)) = reduce ((a*x) /- (b*y, c*z))
=======
infixr 5 ++*
(++*) :: Fraction Int (Int, Int) -> Fraction Int (Int, Int) -> Fraction Int (Int, Int)
(Fraction a (b,c)) ++* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))
>>>>>>> 607d54ae36e9bbbc6965dc91145ac1982efc4d19
=======
infixr 5 ++*
(++*) :: Fraction Int (Int, Int) -> Fraction Int (Int, Int) -> Fraction Int (Int, Int)
(Fraction a (b,c)) ++* (Fraction x (y,z)) = reduce ((a*x) /- (b*y, c*z))
>>>>>>> parent of 4091cd9... æŧįŽå­ãŽååå¤æ´


-- ņĒˇéÖ
reduce :: Fraction Int (Int, Int) -> Fraction Int (Int, Int)
reduce (Fraction a (b,c)) = Fraction a (b `div` (gcd b c), c `div` (gcd b c))

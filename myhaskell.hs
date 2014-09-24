{-# OPTIONS -Wall -Werror #-}

-- �ѕ���
data Fraction a = Fraction a (a, a)
 deriving (Show, Read, Eq, Ord)

-- �ѕ��������߂�֐�
infixl 9 /-
(/-) :: Int -> (Int, Int) -> Fraction Int
x /- (y,z)
	| y < z = Fraction x (y,z)
	| y < 0 = (x - 1) /- (y + z, z)
	| otherwise = (x + 1) /- (y-z, z)

-- Fraction Int (Int, Int)���m�̑����Z
infixr 5 /-:+
(/-:+) :: Fraction Int -> Fraction Int -> Fraction Int
(Fraction a (b,c)) /-:+ (Fraction x (y,z)) = reduce ((a+x) /- (b*z + c*y, c*z))

-- Fraction Int (Int, Int)���m�̈����Z
infixr 5 /-:-
(/-:-) :: Fraction Int -> Fraction Int -> Fraction Int
(Fraction a (b,c)) /-:- (Fraction x (y,z)) = reduce ((a-x) /- (b*z - c*y, c*z))

-- Fraction Int (Int, Int)���m�̊|���Z
<<<<<<< HEAD
infixr 5 ++*
(++*) :: Fraction Int (Int, Int) -> Fraction Int (Int, Int) -> Fraction Int (Int, Int)
(Fraction a (b,c)) ++* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))
=======
infixr 6 /-:*
(/-:*) :: Fraction Int -> Fraction Int -> Fraction Int
(Fraction a (b,c)) /-:* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))
>>>>>>> 104a7ddc03c44e066957bf1d4bf00c2c77bcebeb


-- �񕪂���֐�
reduce :: Fraction Int -> Fraction Int
reduce (Fraction a (b,c)) = Fraction a (b `div` (gcd b c), c `div` (gcd b c))

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
infixr 6 /-:*
(/-:*) :: Fraction Int -> Fraction Int -> Fraction Int
(Fraction a (b,c)) /-:* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))


-- �񕪂���֐�
reduce :: Fraction Int -> Fraction Int
reduce (Fraction a (b,c)) = Fraction a (b `div` (gcd b c), c `div` (gcd b c))

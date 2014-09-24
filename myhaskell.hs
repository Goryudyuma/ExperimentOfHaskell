{-# OPTIONS -Wall -Werror #-}

-- �ѕ���
data Fraction = Fraction Int (Int, Int)
 deriving (Show, Read, Eq, Ord)

-- �ѕ��������߂�֐�
infixl 9 /-
(/-) :: Int -> (Int, Int) -> Fraction
x /- (y,z)
	| y < z = Fraction x (y,z)
	| y < 0 = (x - 1) /- (y + z, z)
	| otherwise = (x + 1) /- (y-z, z)

-- Fraction���m�̑����Z
infixr 5 /-:+
(/-:+) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:+ (Fraction x (y,z)) = reduce ((a+x) /- (b*z + c*y, c*z))

-- Fraction���m�̈����Z
infixr 5 /-:-
(/-:-) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:- (Fraction x (y,z)) = reduce ((a-x) /- (b*z - c*y, c*z))

-- Fraction���m�̊|���Z
infixr 6 /-:*
(/-:*) :: Fraction -> Fraction -> Fraction
(Fraction a (b,c)) /-:* (Fraction x (y,z)) = reduce (0 /- (a*c*x*z+b*x*z+a*c*y+b*y, c*z))


-- �񕪂���֐�
reduce :: Fraction -> Fraction
reduce (Fraction a (b,c)) = Fraction a (b `div` (gcd b c), c `div` (gcd b c))

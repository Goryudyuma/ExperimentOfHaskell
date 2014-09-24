{-# OPTIONS -Wall -Werror #-}

data Ternary = Unknown | True' | False' deriving (Eq)
instance Show Ternary where
	show Unknown = "Unknown"
	show True' = "True"
	show _ = "False"

infixl 3 `and'`
and' :: Ternary -> Ternary -> Ternary
x `and'` y
	|x==Unknown || y==Unknown = Unknown
	|x==True' && y==True' =True'
	|otherwise = False'

infixl 2 `or'`
or' :: Ternary -> Ternary -> Ternary
x `or'` y
	|x==Unknown || y==Unknown = Unknown
	|x==True' || y==True' =True'
	|otherwise = False'

not' :: Ternary -> Ternary
not' x 
	|x==Unknown = Unknown
	|x==True' =False'
	|otherwise = True'

infixl 1 `nand'`
nand' :: Ternary -> Ternary -> Ternary
x `nand'` y = not' (x `and'` y) 

infixl 1 `nor'`
nor' :: Ternary -> Ternary -> Ternary
x `nor'` y = not' (x `or'` y)

infixl 1 `xor'`
xor' :: Ternary -> Ternary -> Ternary
x `xor'` y = (x `and'` y ) `and'` (x `nor'` y)

infixl 1 `xnor'`
xnor' :: Ternary -> Ternary -> Ternary
x `xnor'` y = not' (x `xor'` y)
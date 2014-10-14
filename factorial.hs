{-# OPTIONS -Wall -Werror #-}

import System.Environment
import Control.Applicative

main :: IO ()
main = do
	(input : _) <- getArgs
	print $ factorial (read input :: Integer)

factorial :: Integer -> Maybe Integer
factorial x
	| x < 0 = Nothing
	| x == 0 = Just 1
	| otherwise = pure (*) <*> Just x <*> factorial (x - 1)

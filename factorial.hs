{-# OPTIONS -Wall -Werror #-}

import System.Environment
import Control.Applicative

main :: IO ()
main = do
	(input : _) <- getArgs
	print $ factorial (read input :: Integer)

factorial :: (Integral a) => a -> Maybe a
factorial x
	| x < 0 = Nothing
	| x == 0 = Just 1
	| otherwise = (*x) <$> factorial (x - 1)

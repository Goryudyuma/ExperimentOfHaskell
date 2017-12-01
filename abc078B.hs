import Control.Applicative
 
main = do
  input <- (map (read ::String->Int) . words) <$> getLine 
  putStrLn $ show $  (input !! 0 - input !! 2) `div` (input !! 1 + input !! 2) 

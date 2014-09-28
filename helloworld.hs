{-# OPTIONS -Wall -Werror #-}

import System.Environment

main :: IO ()
main = do
	args <- getArgs
	flip writeFile helloC $ case args of
		[] -> "helloworld.c"
		_ -> head args ++ ".c"

helloC :: String
helloC = "#include <stdio.h>\nint main(void) {\n\tprintf(\"Hello, world\\n\");\n\treturn 0;\n}"

{-# OPTIONS -Wall -Werror #-}

main :: IO ()
main = writeFile "helloworld.c" helloC

helloC :: String
helloC = "#include <stdio.h>\nint main(void) {\n\tprintf(\"Hello, world\\n\");\n\treturn 0;\n}"

{-# OPTIONS -Wall -Werror #-}

import Graphics.UI.GLUT

main :: IO ()
main = do
	_ <- initialize "Test Program" [""]
	_ <- createWindow "Test Window"
	displayCallback $= return ()
	mainLoop


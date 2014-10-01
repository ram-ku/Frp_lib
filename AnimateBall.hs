module AnimateBall where

import Frp_lib
import Animator
import Graphics.Blank

main :: IO ()
main = blankCanvas 3000 $ \ context -> loop context $ bouncingBall ball1
loop context (n:ns) = do{
						send context $ do {
							render (renderBall n) };
						loop context ns}

renderBallList :: [Ball] -> Canvas ()
renderBallList  [] = do return ()
renderBallList  (b:xs) = do 
					x <- scaleX (ballPosX b)
					y <- scaleY (0.8)
					r <- scaleLength (ballRad b)
					circle x y r "Blue"
					renderBallList xs 

renderBall :: Ball -> Canvas ()
renderBall  b = do {
					x <- scaleX (ballPosX b);
					y <- scaleY (ballPosY b);
					r <- scaleLength (ballRad b);
					circle x y r "Blue";
					}
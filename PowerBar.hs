module PowerBar where
import Graphics.Blank

type RGB = String
type RecX = Float
type RecY = Float

-- The HSV gradient from red to green
powerBar :: [RGB]
powerBar = ["#FF0000","#FF2B00","#FF5500", "#FF8000","#FFAA00","#FFD500","#FFFF00","#D5FF00", "#AAFF00", "#80FF00", "#55FF00", "#2BFF00", "#00FF00"]

barX :: Float
barX = 400

barY :: Float
barY = 200


barHeight :: Float
barHeight = 10

barWidth :: Float
barWidth = 40

rect :: Float -> Float -> Float -> Float -> String -> Canvas ()
rect x y w h col = do 
	beginPath ()
	fillStyle col
	fillRect (x, y, w, h)
	closePath()

-- pre:: an Int larger than 0 
-- draw i rectangulars with corresponding coloring
rects :: Int -> RecX -> RecY -> Canvas ()
rects 0 _ _ = return ()
rects i x y = do
	(width,height) <- size
	rect x y (width*0.1) barHeight (powerBar !! ( i-1 ) )
	rects (i-1) x (y+10)

renderPowerBar :: Float -> RecX -> RecY -> Canvas ()
renderPowerBar flowR x y 
		| (flowR <= 1/13.0) = rects 1 x y 
		| (flowR <= 2/13.0) = rects 2 x (y - barHeight*1)
		| (flowR <= 3/13.0) = rects 3 x (y - barHeight*2)
		| (flowR <= 4/13.0) = rects 4 x (y - barHeight*3)
		| (flowR <= 5/13.0) = rects 5 x (y - barHeight*4)
		| (flowR <= 6/13.0) = rects 6 x (y - barHeight*5)
		| (flowR <= 7/13.0) = rects 7 x (y - barHeight*6)
		| (flowR <= 8/13.0) = rects 8 x (y - barHeight*7)
		| (flowR <= 9/13.0) = rects 9 x (y - barHeight*8)
		| (flowR <= 10/13.0) = rects 10 x (y - barHeight*9)
		| (flowR <= 11/13.0) = rects 11 x (y - barHeight*10)
		| (flowR <= 12/13.0) = rects 12 x (y - barHeight*11)
		| (flowR <= 13/13.0) = rects 13 x (y - barHeight*12)


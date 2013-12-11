import PowerBar
import GraphConstruction

import Data.Maybe
import Control.Concurrent
import Data.Char
import Data.List.Split
import Graphics.Blank
import System.Environment  

lineColor :: String
lineColor = "#00FFFF"

ballRad :: Radius
ballRad = 10

scaleX :: Float -> Float -> Float 
scaleX x w = (x * w / areaLength) * 0.8 + 0.1 * w

scaleY :: Float -> Float -> Float
scaleY y h = (h - (y * h / areaLength)) * 0.8 + 0.1 * h

scaleR :: Float -> Float -> Float
scaleR r c = (r * c / areaLength) * 0.8

constructBallList :: [String] -> [Ball]
constructBallList = map parseNode

constructTBall :: [String] -> [[Ball]]
constructTBall [] = []
constructTBall xs = (constructBallList current) : (constructTBall rest)
      where
        current = take nWifi xs 
        rest    = drop nWifi xs

-- parse a line of original strings into a node
parseNode :: String -> Ball
parseNode xs = MkBall i x y ballRad "#FFD473"
  where 
    parsedList = splitOneOf "=+: n" xs
    --t = read (parsedList !! 3)  :: Float
    i = read (parsedList !! 7) :: Int
    x = read (parsedList !! 9)  :: Float
    y = read (parsedList !! 10) :: Float

-- format the transmission range circle 
rangeCircle :: NodeID -> XCo -> YCo ->  Radius -> Color -> Canvas ()
rangeCircle i x y r col = do 
  (width,height) <- size
  fillStyle "rgba(250,212,210,0.1)"
  beginPath ()
  arc (scaleX x height, scaleY y height, (scaleR r height - ballRad/2), 0, pi*2, False)
  fill ()
  lineWidth 2
  strokeStyle "rgba(250,212,110,0.2)"  
  stroke()
  closePath ()

-- format the node
circle :: NodeID -> XCo -> YCo ->  Radius -> Color -> Canvas ()
circle i x y r col = do 
  (width,height) <- size
  fillStyle col
  beginPath ()
  arc (scaleX x height, scaleY y height, r, 0, pi*2, False)
  fill ()
  closePath ()
  fillStyle "#FF7F00"
  font "8pt Calibri"
  fillText(show i, scaleX (x-4) height, scaleY (y-2) height)
                    
-- format the line                
line :: XCo -> YCo -> XCo -> YCo -> Canvas ()
line x1 y1 x2 y2 = do
  (width,height) <- size
  beginPath ()
  moveTo (scaleX x1 height, scaleY y1 height)
  lineTo (scaleX x2 height, scaleY y2 height)
  lineWidth 2
  lineCap "round"
  strokeStyle lineColor           
  stroke()
  closePath ()

--  render text into Canvas
renderText :: String -> Float -> Float ->Canvas ()
renderText str tx ty = do
  fillStyle "Blue"
  beginPath ()
  font "10pt Calibri"
  fillText(str, tx, ty)
  closePath ()

-- render one node into Canvas
renderBall :: Ball -> Canvas ()
renderBall b = do
  circle (nid b) (xco b) (yco b) (radius b) (color b)
  rangeCircle (nid b) (xco b) (yco b) tranRange (color b)

-- render all nodes into Canvas
renderBalls :: [Ball] -> Canvas ()
renderBalls []     = return ()
renderBalls (b:bs) = do renderBall b
                        renderBalls bs

-- render one link into Canvas
renderLine :: Line -> Canvas ()
renderLine l =  line (startX l) (startY l) (endX l) (endY l)
                      
-- render all links into Canvas
renderLines :: [Line] -> Canvas ()
renderLines [] = return ()
renderLines (l:ls) = do 
                    renderLine l
                    renderLines ls

outputTest :: IO() 
outputTest = do 
  contents <- getContents
  print (constructTBall $ (lines contents))

restart :: [[Ball]] -> Context -> IO()
restart tb context = do
    event <- send context $ readEvent MouseDown
    case jsMouse event of
      Nothing -> do
        send context $ do
          (width,height) <- size
          clearRect (0,0,width,height)
      Just (x,y) -> loop context tb 0
    restart tb context

drawCanvas :: Context -> [[Ball]] -> Int -> IO()
drawCanvas context bs n = do 
      send context $ do
        (width,height) <- size
        clearRect (0,0,width,height)
        renderLines (findLines $ (bs!!n))
        renderBalls (bs!!n)
        let flow_robust = (flowRobust $ mkGraph $ reducedPairs $ (bs!!n) )
        renderPowerBar flow_robust (width*0.8) (height*0.8)
        renderText "Flow Robustness" (width*0.81) (height*0.85)
        renderText ("time [0.1s]: " ++ show n) (width*0.81) (height*0.1)


-- jscode of keys: F(arward) -> 70, B(ack) -> 66, S(top) -> 83
stop :: Context -> [[Ball]] -> Int -> IO()
stop context bs n = 
    do
      drawCanvas context bs n 
      event <- send context $ tryReadEvents [KeyDown]
      case event of
        Just (NamedEvent KeyDown e) -> do 
          case jsCode e of 
            66 -> rloop context bs n
            70 -> loop context bs (n+1)
            _ -> stop context bs n
        Nothing -> do
          stop context bs n

loop :: Context -> [[Ball]] -> Int -> IO()
loop context bs n = 
    do
      drawCanvas context bs n 
      event <- send context $ tryReadEvents [KeyDown]
      case event of
        Just (NamedEvent KeyDown e) -> do 
          case jsCode e of 
            66 -> rloop context bs n
            83 -> stop context bs n
            _  -> loop context bs (n+1)
        Nothing -> do
          loop context bs (n+1)

-- program needs to be reinitiated if going back the 0 time
rloop :: Context -> [[Ball]] -> Int -> IO()
rloop context bs n = 
    do
      drawCanvas context bs n 
      event <- send context $ tryReadEvents [KeyDown]
      case event of 
        Just (NamedEvent KeyDown e) -> do 
          case jsCode e of 
            70 -> loop context bs n
            83 -> stop context bs n
            _  -> rloop context bs (n-1)
        Nothing -> do
          rloop context bs (n-1)

main = do
  args <- getArgs
  blankCanvas (read (args!!0) ::Int) $ \ context -> do
    contents <- getContents
    let tballs = constructTBall $ (lines contents)
    loop context tballs (0 :: Int)
    restart tballs context 



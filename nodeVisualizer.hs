import Data.Maybe
import Control.Concurrent
import Data.Char
import Data.List.Split
import Graphics.Blank
import PowerBar
import System.Environment  
import Data.Graph
import Data.Tree

type NodeID = Int
type XCo    = Float
type YCo    = Float
type ZCo    = Float
type Radius = Float
type Distance = Float
type Color  = String
type Time 	= Float

data Ball = MkBall { 
                    nid :: NodeID,
					          xco :: XCo,
                    yco :: YCo,
                    radius :: Radius,
                    color :: Color
                   } deriving (Eq, Ord, Show)  

data Line = MkLine  {
                      startX :: XCo,
                      startY :: YCo,
                      endX :: XCo,
                      endY :: YCo
                    } deriving (Eq, Ord, Show) 

lineColor :: String
lineColor = "#00FFFF"

nWifi :: Int
nWifi = 20

nNeighbor :: Float
nNeighbor = 4.0

gBound :: Bounds
gBound = (0, nWifi - 1)

tranRange :: Float
tranRange = 100.0

ballRad :: Radius
ballRad = 10
--sqrt(m_nWifi*3.14*pow(m_maxRange, 2)/m_nNeighb);
areaLength :: Float
areaLength = sqrt(fromIntegral(nWifi)*3.14*(tranRange**2)/nNeighbor)

scaleX :: Float -> Float -> Float 
scaleX x w = (x * w / areaLength) * 0.8 + 0.1 * w

scaleY :: Float -> Float -> Float
scaleY y h = (h - (y * h / areaLength)) * 0.8 + 0.1 * h

scaleR :: Float -> Float -> Float
scaleR r c = (r * c / areaLength) * 0.8

constructBallList :: [String] -> [Ball]
constructBallList [] = []
constructBallList (x:xs) = (parseNode x) : (constructBallList xs)

constructTBall :: [String] -> [[Ball]] -- TBall
constructTBall [] = []
constructTBall xs = (constructBallList current) : (constructTBall rest)
			where
				current = take nWifi xs 
				rest    = drop nWifi xs


parseNode :: String -> Ball
parseNode xs = MkBall i x y ballRad "#FFD473"
	where 
		parsedList = splitOneOf "=+: n" xs
		--t = read (parsedList !! 3)  :: Float
		i = read (parsedList !! 7) :: Int
		x = read (parsedList !! 9)  :: Float
		y = read (parsedList !! 10) :: Float

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
                      
renderText :: String -> Float -> Float ->Canvas ()
renderText str tx ty = do
  fillStyle "Blue"
  beginPath ()
  font "10pt Calibri"
  fillText(str, tx, ty)
  closePath ()

renderBall :: Ball -> Canvas ()
renderBall b = do
  circle (nid b) (xco b) (yco b) (radius b) (color b)
  rangeCircle (nid b) (xco b) (yco b) tranRange (color b)

renderBalls :: [Ball] -> Canvas ()
renderBalls []     = return ()
renderBalls (b:bs) = do renderBall b
                        renderBalls bs

-- how to get Maybe Line to Line 
renderLine :: Line -> Canvas ()
renderLine l =  line (startX l) (startY l) (endX l) (endY l)
                      

renderLines :: [Line] -> Canvas ()
renderLines [] = return ()
renderLines (l:ls) = do 
                    renderLine l
                    renderLines ls

computeDis :: (Ball, Ball) -> Distance
computeDis (b1, b2) = sqrt $ (xco b1 - xco b2)**2 + (yco b1 - yco b2)**2
                  
addLine :: (Ball, Ball) -> Line
addLine (b1, b2) =  MkLine (xco b1) (yco b1) (xco b2) (yco b2) 

-- remove (1, 2) <=> (2, 1) redundent edges and remove edge out of range
reduceAdj :: [(Ball, Ball)] -> [(Ball, Ball)]
reduceAdj [] = []
reduceAdj (p:ps) =  
  if ( (nid (fst p)) < (nid (snd p)) && dis < 100 )  then
    p : reduceAdj ps
  else
    reduceAdj ps 
  where
    dis = computeDis (fst p, snd p)

reducedPairs :: [Ball] -> [(Ball, Ball)]
reducedPairs bs = reduceAdj [(b1,b2) | b1 <- bs, b2 <- bs]

cleanLines :: [Line] -> [Line]
cleanLines [] = []
cleanLines (l:ls) = 
  if ( (startX l) == -1 ) then
    l : cleanLines ls
  else
    cleanLines ls
         
findLines :: [Ball] -> [Line]
findLines bs = fmap addLine (reducedPairs bs)

---- construct a graph
getEdges ::  [(Ball,Ball)] -> [Edge]
getEdges [] = []
getEdges pairs = (nid (fst (head pairs)), nid (snd (head pairs)) ): getEdges (tail pairs)

mkGraph :: [(Ball,Ball)] -> Graph
mkGraph pairs = buildG gBound (getEdges pairs)

flows :: Forest Vertex -> Int
flows [] = 0
flows (c:cs) = ((length (flatten c)) * (length (flatten c)-1)) + flows cs  

maxFlow :: Bounds -> Int
maxFlow b = (snd b) * (snd b + 1)

flowRobust :: Graph -> Float
flowRobust aGraph = fromIntegral(flows $ components aGraph) / fromIntegral (maxFlow gBound)
 
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

loop :: Context -> [[Ball]] -> Int -> IO()
loop context bs n = 
    do
      q <- events context MouseDown
      drawCanvas context bs n 
      me <- tryReadEventQueue q
      case me >>= jsMouse of
        Nothing -> do
          if (n < 1000) then
            loop context bs (n+1)
          else
            send context $ do
              (width,height) <- size
              fillStyle "Blue"
              beginPath ()
              font "100pt Georgia"
              fillText("TIME OUT", (width*0.1), (height*0.5))
              closePath ()

        Just (x,y) -> do 
          rloop context bs n

rloop :: Context -> [[Ball]] -> Int -> IO()
rloop context bs n = 
    do
      q <- events context MouseDown
      drawCanvas context bs n 
      me <- tryReadEventQueue q
      case me >>= jsMouse of
        Nothing -> do
          if (n > 0) then
            rloop context bs (n-1)
          else
            send context $ do
              (width,height) <- size
              fillStyle "Blue"
              beginPath ()
              font "100pt Georgia"
              fillText("BackToStart", (width*0.1), (height*0.5))
              closePath ()
        Just (x,y) -> do 
          loop context bs n

main = do
  args <- getArgs
  blankCanvas (read (args!!0) ::Int) $ \ context -> do
    contents <- getContents
    let tballs = constructTBall $ (lines contents)
    loop context tballs (0 :: Int)
    restart tballs context 



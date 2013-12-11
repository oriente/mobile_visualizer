module GraphConstruction where

import Data.Graph
import Data.Tree

type NodeID   = Int
type XCo      = Float
type YCo      = Float
type ZCo      = Float
type Radius   = Float
type Distance = Float
type Color    = String

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

-- the number of nodes in the network
nWifi :: Int
nWifi = 20

-- the average number of neighbor of each node, this is used to determine network density
nNeighbor :: Float
nNeighbor = 4.0

gBound :: Bounds
gBound = (0, nWifi - 1)

-- transmission range of each node
tranRange :: Float
tranRange = 100.0

-- sqrt(m_nWifi*3.14*pow(m_maxRange, 2)/m_nNeighb);
-- the edge length of square simulation area
areaLength :: Float
areaLength = sqrt(fromIntegral(nWifi)*3.14*(tranRange**2)/nNeighbor)

-- compute the distance between two nodes
computeDis :: (Ball, Ball) -> Distance
computeDis (b1, b2) = sqrt $ (xco b1 - xco b2)**2 + (yco b1 - yco b2)**2
                  
-- add a line between nodes
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

-- obtain all the node pairs that are within the transmission range of each other
reducedPairs :: [Ball] -> [(Ball, Ball)]
reducedPairs bs = reduceAdj [(b1,b2) | b1 <- bs, b2 <- bs]

-- obtain all the links based on current node position
findLines :: [Ball] -> [Line]
findLines bs = fmap addLine (reducedPairs bs)
         
-- construct a graph
getEdges ::  [(Ball,Ball)] -> [Edge]
getEdges [] = []
getEdges pairs = (nid (fst (head pairs)), nid (snd (head pairs)) ): getEdges (tail pairs)

-- construct a graph from a list of edges
mkGraph :: [(Ball,Ball)] -> Graph
mkGraph pairs = buildG gBound (getEdges pairs)

-- compute the reliable flows existing in current topology instance
flows :: Forest Vertex -> Int
flows [] = 0
flows (c:cs) = ((length (flatten c)) * (length (flatten c)-1)) + flows cs  

-- the maximum possible flows in the network
maxFlow :: Bounds -> Int
maxFlow b = (snd b) * (snd b + 1)

-- flow robustness of current topology instance
flowRobust :: Graph -> Float
flowRobust aGraph = fromIntegral(flows $ components aGraph) / fromIntegral (maxFlow gBound)

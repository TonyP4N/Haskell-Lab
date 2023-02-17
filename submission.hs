{-ex1-}
-- define a datatype for different colours
data Colour = Black | White
  deriving (Eq, Show)

-- define a datatype for a tree of colours
data ColourTree = Leaf Colour | Branch (ColourTree) (ColourTree) (ColourTree) (ColourTree)
  deriving (Eq, Show)

-- define a function that returns a tree of a given size with all black leaves
allBlack :: Int -> ColourTree
allBlack n = Leaf Black

-- define a function that returns a tree of a given size with all white leaves
allWhite :: Int -> ColourTree
allWhite n = Leaf White

-- define a function that rotates a tree clockwise
clockwise :: ColourTree -> ColourTree -> ColourTree -> ColourTree -> ColourTree
clockwise a b c d = Branch (a) (b) (d) (c)

-- define a function that rotates a tree anticlockwise
anticlockwise :: ColourTree -> ColourTree -> ColourTree -> ColourTree -> ColourTree
anticlockwise a b c d = Branch (a) (d) (b) (c)

{-ex2-}
--define a datatype for a tree of neighbours
data TreeNeighbours = Cell Colour [Colour] | Quadrant TreeNeighbours TreeNeighbours TreeNeighbours TreeNeighbours
  deriving (Eq, Show)

-- define a datatype that represents neighbours from different directions
data Neighbour = Empty | NQuadtree ColourTree
  deriving (Eq, Show)

data Neighbours = Neighbours {
    top :: Neighbour,
    bottom :: Neighbour,
    left :: Neighbour,
    right :: Neighbour}
 deriving (Eq, Show)

-- define functions that get a single cell or a grid of cells from a Neighbour
getGridTL :: Neighbour -> Neighbour
getGridTL Empty = Empty
getGridTL (NQuadtree (Leaf colour)) = NQuadtree (Leaf colour)
getGridTL (NQuadtree (Branch a b c d)) = NQuadtree a
getGridTR :: Neighbour -> Neighbour
getGridTR Empty = Empty
getGridTR (NQuadtree (Leaf colour)) = NQuadtree (Leaf colour)
getGridTR (NQuadtree (Branch a b c d)) = NQuadtree b
getGridBL :: Neighbour -> Neighbour
getGridBL Empty = Empty
getGridBL (NQuadtree (Leaf colour)) = NQuadtree (Leaf colour)
getGridBL (NQuadtree (Branch a b c d)) = NQuadtree c
getGridBR :: Neighbour -> Neighbour
getGridBR Empty = Empty
getGridBR (NQuadtree (Leaf colour)) = NQuadtree (Leaf colour)
getGridBR (NQuadtree (Branch a b c d)) = NQuadtree d

--define functions that get neighbours from different directions
topEdge :: Neighbour -> [Colour]
topEdge Empty = []
topEdge (NQuadtree (Leaf colour)) = [colour]
topEdge (NQuadtree (Branch a b c d)) = topEdge (NQuadtree a) ++ topEdge (NQuadtree b)

botEdge :: Neighbour -> [Colour]
botEdge Empty = []
botEdge (NQuadtree (Leaf colour)) = [colour]
botEdge (NQuadtree (Branch a b c d)) = botEdge (NQuadtree c) ++ botEdge (NQuadtree d)

leftEdge :: Neighbour -> [Colour]
leftEdge Empty = []
leftEdge (NQuadtree (Leaf colour)) = [colour]
leftEdge (NQuadtree (Branch a b c d)) = leftEdge (NQuadtree a) ++ leftEdge (NQuadtree c)

rightEdge :: Neighbour -> [Colour]
rightEdge Empty = []
rightEdge (NQuadtree (Leaf colour)) = [colour]
rightEdge (NQuadtree (Branch a b c d)) = rightEdge (NQuadtree b) ++ rightEdge (NQuadtree d)

--update NQuadtree(outside)
concatNeighsOut :: TreeNeighbours -> Neighbours -> TreeNeighbours
concatNeighsOut (Cell colour neighColours) neighbours = Cell colour (neighColours ++
      topEdge (bottom neighbours) ++
      botEdge (top neighbours) ++
      leftEdge (right neighbours) ++
      rightEdge (left neighbours))
--consider the 4 quadrants separately
concatNeighsOut (Quadrant a b c d) neighbours = Quadrant (concatNeighsOut a (Neighbours {
    top = getGridBL (top neighbours),
    bottom = Empty,
    left = getGridTR (left neighbours),
    right = Empty}))
    (concatNeighsOut b (Neighbours {
    top = getGridBR (top neighbours),
    bottom = Empty,
    left = Empty,
    right = getGridTL (right neighbours)}))
    (concatNeighsOut c (Neighbours {
    top = Empty,
    bottom = getGridTL (bottom neighbours),
    left = getGridBR (left neighbours),
    right = Empty}))
    (concatNeighsOut d (Neighbours {
    top = Empty,
    bottom = getGridTR (bottom neighbours),
    left = Empty,
    right = getGridBL (right neighbours)}))

--update NQuadtree(inside)
concatNeighsIn :: ColourTree -> TreeNeighbours
concatNeighsIn (Leaf colour) = Cell colour []
concatNeighsIn (Branch a b c d) = Quadrant (concatNeighsOut (concatNeighsIn a) (Neighbours {
    top = Empty,
    bottom = NQuadtree c,
    left = Empty,
    right = NQuadtree b}))
    (concatNeighsOut (concatNeighsIn b) (Neighbours {
    top = Empty,
    bottom = NQuadtree d,
    left = NQuadtree a,
    right = Empty}))
    (concatNeighsOut (concatNeighsIn c) (Neighbours {
    top = NQuadtree a,
    bottom = Empty,
    left = Empty,
    right = NQuadtree d}))
    (concatNeighsOut (concatNeighsIn d) (Neighbours {
    top = NQuadtree b,
    bottom = Empty,
    left = NQuadtree c,
    right = Empty}))

--define functions that count the number of different colours in a list
-- if the colour is black, add 1 to the counter
countBlack :: [Colour] -> Int
countBlack [] = 0
countBlack (x:xs) = if x == Black then 1 + countBlack xs else countBlack xs
-- if the colour is white, add 1 to the counter
countWhite :: [Colour] -> Int
countWhite [] = 0
countWhite (x:xs) = if x == White then 1 + countWhite xs else countWhite xs

--decide whether to turn the colour of a cell to black or white
-- if the colour is black and more than half of its neighbours are white, turn it to white
-- if the colour is white and more than half of its neighbours are black, turn it to black
colourDecision :: Colour -> [Colour] -> Colour
colourDecision colour neighColours =
  if colour == Black && countWhite neighColours > length neighColours `div` 2 then White
  else if colour == White && countBlack neighColours > length neighColours `div` 2 then Black
  else colour

-- define a function that turn the colour of tree to black if more than half of its neighbours are black
-- use colourDecision to decide whether to turn the colour of a cell to black or white
updateColour :: TreeNeighbours -> ColourTree
updateColour (Cell colour neighColours) = Leaf (colourDecision colour neighColours)
updateColour (Quadrant a b c d) = Branch (updateColour a) (updateColour b) (updateColour c) (updateColour d)

-- define blur function
--use concatNeighsIn and concatNeighsOut to get the neighbours of each cell
--use updateColour to turn the colour of each cell
blur :: ColourTree -> ColourTree
blur (Leaf colour) = Leaf colour
blur (Branch a b c d) = updateColour (concatNeighsIn (Branch a b c d))

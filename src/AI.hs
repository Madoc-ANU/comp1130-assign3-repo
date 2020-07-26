{-|
Module      : AI
Description : AIs for Ataxx
Copyright   : (c) 2020 Your Name Here
License     : AllRightsReserved
-}
module AI where

import Ataxx

-- | Type of AI functions you can choose to write.
data AIFunc
  = NoLookahead (GameState -> Move)
    -- ^ Simple AIs that do not need lookahead.
  | WithLookahead (GameState -> Int -> Move)
    -- ^ AIs that want to look ahead. The assignment framework will
    -- call the function over and over with increasing integer
    -- arguments @1, 2, 3, ...@ until your AI's time limit is up.

-- | The table of all AIs that your assignment provides. The AI named
-- "default" in this table is the one your tutor will dedicate most of
-- his or her attention to marking.
ais :: [(String, AIFunc)]
ais = [ ("firstLegalMove", NoLookahead firstLegalMove), ("protoType", NoLookahead protoType)
      ]

firstLegalMove :: GameState -> Move
firstLegalMove st = head (legalMoves st)

protoType :: GameState -> Move
protoType st = head (moveSelect 10 (legalMoves st))

-- | A very simple AI, which picks the first move returned by the
-- 'legalMoves' function. AIs can rely on the 'legalMoves' list being
-- non-empty; if there were no legal moves, the framework would have
-- ended the game.


--accessors
getL1 (Move l1 _) = l1
getL2 (Move _ l2) = l2

getX (Location x _) = x
getY (Location _ y) = y

--main functions

caclHeur :: GameState -> Int
caclHeur st = fst (countPieces st) - snd (countPieces st) + (squareValue st)

squareValue :: GameState -> Int
squareValue st = countFunction 0 0 (board st)

countFunction :: Int -> Int -> Board -> Int
countFunction acc _ [] = acc
countFunction acc ref (x:xs) = case ref of
  0 -> countFunction acc (ref+1) xs
  8 -> countFunction acc (ref+1) xs
  _ -> countFunction (acc+1) (ref+1) xs

countLine :: Int -> Int -> [Square] -> Int
countLine acc _ [] = acc
countLine acc ref (x:xs) = case ref of
  0 -> countFunction acc (ref+1) xs
  8 -> countFunction acc (ref+1) xs
  _ -> countFunction (acc+1) (ref+1) xs

readSquare :: Square -> Int
readSquare sq = case sq of
  Piece Player1 -> 0
  Piece Player2 -> 1
  Block -> 2
  Empty -> 3

  --EvalMovesSystem

readMove :: Move -> Int
readMove _ = 0

evalMoveList :: [Move] -> [Int]
evalMoveList (x:xs) output = (readMove x):output

findHighest :: [Int] -> Int -> Int
findHighest _ high ref = high
findHighest (x:xs) 0 ref = x
findHighest (x:xs) high ref = case x of
  x > high -> find
  _ -> findHighest (x:xs)

moveSelect :: Integer -> [Move] -> [Move]
moveSelect _ [] = []
moveSelect 1 (x:xs) = x:[]
moveSelect n (x:xs) = moveSelect (n-1) xs

--Location -> square

locSquare :: GameState -> Location -> Square
locSquare st (Location x y) = lookup x y (board st)

lookup :: Int -> Int -> Board -> Square
lookup x 0 (n:ns) = find x n
lookup x y (n:ns) = lookup (x) (y-1) (n:ns)

find :: Int -> [Square] -> Square
find 0 (n:ns) = n
find x (n:ns) = find (x-1) (n:ns)

--How many enemies next to locatoin?
adjSquares :: Location -> [Location] -> [Location]
adjSquares (Location x y) output = do
  if onBoard st (Location (x+1) y)
    then do(Location (x+1) y):output
    else
  if onBoard st (Location (x-1) y)
    then do(Location (x-1) (y)):output
    else
  if onBoard st (Location (x+1) (y-1))
    then do(Location (x+1) y):output
    else
  if onBoard st (Location (x-1) (y-1))
    then do(Location (x-1) (y)):output
    else
  if onBoard st (Location (x+1) (y+1))
    then do(Location (x+1) y):output
    else
  if onBoard st (Location (x-1) (y+1))
    then do(Location (x-1) (y)):output
    else
  if onBoard st (Location (x) (y+1))
    then do(Location (x+1) y):output
    else
  if onBoard st (Location (x) (y+1))
    then do(Location (x-1) (y)):output
    else
  return output

adjSquares2 :: Location -> [Location] -> [Location]
adjSquares2 l (x:xs) = if (chebyshev l) == 1 do x:adjSquares l xs else do adjSquares xs

  Location x y -> [ Location (x + x') (y + y')
                  | x' <- [-1..1]
                  , y' <- [-1..1]
                  , x' /= 0 || y' /= 0
                  ]


enCount :: GameState -> Location -> Int
enCount st (Location x y) = enCheck(lookup x y (board st))

{-|could use case of here?
genAdjacent :: GameState -> Int -> Int -> [Square]
genAdjacent st x y = []
-}

squareCycle :: [Square] -> Int
squareCycle (x:xs) = enChekc(x) + squareCycle xs

enCheck :: Square -> Int
enCheck s = case s of
  Piece Player2 -> 1
  _ -> 0

--minMax :: Int -> [Move] -> Int
--minMax acc (x:xs) =

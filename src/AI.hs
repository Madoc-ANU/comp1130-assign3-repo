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
readMove _ -> 0

evalMoveList :: [Move] -> [Int]
evalMoveList (x:xs) output = (readMove x):output

findHighest :: [Int] -> Int -> Int
findHighest (x:xs) acc = do

moveSelect :: Integer -> [Move] -> [Move]
moveSelect _ [] = []
moveSelect 1 (x:xs) = x:[]
moveSelect n (x:xs) = moveSelect (n-1) xs

--Location-> square

locSquare :: GameState -> Location -> Square
locSquare st (Location x y) = lookup x y (board st)

lookup :: Int -> Int -> Board -> Square
lookup 0 y (n:ns) = n
lookup x y (n:ns) = lookup (x-1) y (n:ns)

find :: Int -> [Square] -> Square
find 0 (n:ns) = n
find x (n:ns) = find (x-1) (n:ns)

--How many enemies next to locatoin?

enCount :: Location -> Int
enCount l =

genAdjacent :: Int -> Int -> [Square]
genAd

squareCycle ::

enCheck :: Square -> Bool
enCheck s = case s of
  Piece Player2 -> True
  _ -> False

--minMax :: Int -> [Move] -> Int
--minMax acc (x:xs) =

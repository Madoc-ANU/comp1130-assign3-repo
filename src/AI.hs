{-|
Module      : AI
Description : AIs for Ataxx
Copyright   : (c) 2020 Your Name Here
License     : AllRightsReserved
-}
module AI where

import Ataxx

data AIFunc
  = NoLookahead (GameState -> Move)
  | WithLookahead (GameState -> Int -> Move)
ais :: [(String, AIFunc)]
ais = [ ("firstLegalMove", NoLookahead firstLegalMove), ("protoType", NoLookahead protoType)
      ]

firstLegalMove :: GameState -> Move
firstLegalMove st = head (legalMoves st)

protoType :: GameState -> Move
protoType st = head (moveSelect 10 (legalMoves st))

greedy :: GameState -> Move
greedy st = head (moveSelect (evalMoves (legalMoves st) (legalMoves st))

--greedy

evalMoves :: [Moves] -> [Int]
evalMoves [] = []
evalMoves (x:xs) = evalMove x:evalMoves xs

evalMove :: Move -> GameState -> Int
evalMove m st = sumAdjEn (getL2 m) (board st)

sumAdjEn :: Location -> Board -> Int
sumAdjEn ::

readLocations :: [Location] -> Int
readLocations (x:xs) = readSFL x:

readSquareFromLoc :: Location -> GameState -> Int
readSFL l st = readSqaure (locSquare st l)

readSquare :: Square -> Int
readSquare sq = case sq of
  Piece Player2 -> 1
  _ -> 0

--List of Locations adjacent to a location
adjLoc :: Location -> [Location] -> [Location]
locList = loc (x:xs) - chebList loc x:locList xs]

chebList :: Location -> Location -> [Location]
chebList l1 l2 = case (chebshev l1 l2) of
  1 -> [l2]
  0 -> []

--Location -> square

locSquare :: GameState -> Location -> Square
locSquare st (Location x y) = lookup x y (board st)

lookup :: Int -> Int -> Board -> Square
lookup x 0 (n:ns) = find x n
lookup x y (n:ns) = lookup (x) (y-1) (n:ns)

find :: Int -> [Square] -> Square
find 0 (n:ns) = n
find x (n:ns) = find (x-1) (n:ns)

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


  --EvalMovesSystem
evalMoveList :: [Move] -> [Int]
evalMoveList (x:xs) output = (readMove x):output

readMove :: Move -> Int
readMove m = adjSquares (getL1 m)

findHighest :: [Int] -> Int -> Int
findHighest [] high ref = ref
findHighest (x:xs) 0 ref = x
findHighest (x:xs) high ref = if x > high then do findHighest xs x (ref+1) else do findHighest xs high (ref+1)
  x > high -> find
  _ -> findHighest (x:xs)

moveSelect :: Integer -> [Move] -> [Move]
moveSelect _ [] = []
moveSelect 1 (x:xs) = x:[]
moveSelect n (x:xs) = moveSelect (n-1) xs



--How many enemies next to locatoin?
adjSquares :: Location -> [Location] -> [Location]
adjSquares l (x:xs) = if (chebyshev l) == 1 then do x:adjSquares l xs else do adjSquares xs

enCount :: [Location] -> Int
enCount [] = []
enCount (x:xs) = if (enCheck x) == 1 then do x:enCount l xs else do enCount xs

squareCycle :: [Square] -> Int
squareCycle (x:xs) = enChekc(x) + squareCycle xs

enCheck :: Square -> Int
enCheck s = case s of
  Piece Player2 -> 1
  _ -> 0

--minMax :: Int -> [Move] -> Int
--minMax acc (x:xs) =

{-|
adjEn :: Location -> Int
adjEn l = sumAdjEn let
  name = expression
  in expression
-}

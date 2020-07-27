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
ais = [ ("firstLegalMove", NoLookahead firstLegalMove), ("protoType", NoLookahead protoType), ("greedy", NoLookahead greedy)
      ]

firstLegalMove :: GameState -> Move
firstLegalMove st = head (legalMoves st)

protoType :: GameState -> Move
protoType st = upPack(moveSelect 10 (legalMoves st))

greedy :: GameState -> Move
greedy st = unPack(moveSelect (head (evalMoves (legalMoves st) st)) (legalMoves st))

--greedy
moveSelect :: Int -> [Move] -> [Move]
moveSelect _ [] = []
moveSelect 1 (x:xs) = x
moveSelect n (x:xs) = moveSelect (n-1) xs

unPack :: [a] -> a
unPack [] = []
unPack (x:xs) = x


evalMoves :: [Move] -> GameState -> [Int]
evalMoves [] _ = []
evalMoves (x:xs) st = evalMove x st: evalMoves xs st

evalMove :: Move -> GameState -> Int
evalMove m st = sumAdjEn (getL2 m) st

sumAdjEn :: Location -> GameState -> Int
sumAdjEn l st = sumList (readLocations (adjLoc l (moveToL2List(legalMoves st))) st)

moveToL2List :: [Move] -> [Location]
moveToL2List [] = []
moveToL2List (x:xs) = getL2(x) : moveToL2List xs

--Sums list of integers for adjacent game enemies
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

--List of Locations adjacent to a location within legalMoves
adjLoc :: Location  -> [Location] -> [Location]
adjLoc _ [] = []
adjLoc loc (x:xs) = unPack(chebList loc x): adjLoc loc xs

chebList :: Location -> Location -> [Location]
chebList l1 l2 = case (chebyshev l1 l2) of
  1 -> [l2]
  0 -> []


--gives Int list representing squares from locations
readLocations :: [Location] -> GameState -> [Int]
readLocations [] _ = []
readLocations (x:xs) st = readSFL x st :readLocations xs st

readSFL :: Location -> GameState -> Int
readSFL l st = readSquare (locSquare st l)

readSquare :: Square -> Int
readSquare sq = case sq of
  Piece Player2 -> 1
  _ -> 0

--Location -> square

locSquare :: GameState -> Location -> Square
locSquare st (Location x y) = myLookup x y (board st)

myLookup :: Int -> Int -> Board -> Square
myLookup x 0 (n:ns) = find x n
myLookup x y (n:ns) = myLookup (x) (y-1) (n:ns)

find :: Int -> [Square] -> Square
find 0 (n:ns) = n
find x (n:ns) = find (x-1) (n:ns)

--

--accessors
getL1 (Move l1 _) = l1
getL2 (Move _ l2) = l2

getX (Location x _) = x
getY (Location _ y) = y

{-EvalMovesSystem
evalMoveList :: [Move] -> [Int]
evalMoveList (x:xs) output = (readMove x):output

readMove :: Move -> Int
readMove m = adjSquares (getL1 m)
-}


--main functions
{-|}
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

--How many enemies next to locatoin?
adjSquares :: Location -> [Location] -> [Location]
adjSquares l (x:xs) = if (chebyshev l) == 1 then do x:adjSquares l xs else do adjSquares xs

enCount :: [Location] -> Int
enCount [] = []
enCount (x:xs) = if (enCheck x) == 1 then do x:enCount xs else do enCount xs

squareCycle :: [Square] -> Int
squareCycle (x:xs) = enCheck(x) + squareCycle xs

enCheck :: Square -> Int
enCheck s = case s of
  Piece Player2 -> 1
  _ -> 0
-}
--minMax :: Int -> [Move] -> Int
--minMax acc (x:xs) =

{-|
adjEn :: Location -> Int
adjEn l = sumAdjEn let
  name = expression
  in expression
-}

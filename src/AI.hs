{-|
Module      : AI
Description : AIs for Ataxx
Copyright   : (c) 2020 Madoc Thomas Cottle
License     : AllRightsReserved
-}
module AI where

import Ataxx

data AIFunc
  = NoLookahead (GameState -> Move)
  | WithLookahead (GameState -> Int -> Move)
ais :: [(String, AIFunc)]
ais = [ ("firstLegalMove", NoLookahead firstLegalMove)
      ]

firstLegalMove :: GameState -> Move
firstLegalMove st = head (legalMoves st)
{-
protoType :: GameState -> Move
protoType st = head (moveSelect 10 (legalMoves st))

greedy :: GameState -> Move
greedy st = head (moveSelect (head(evalMoves (legalMoves st) st)) (legalMoves st))
-}

wenKroist :: GameState -> Move
wenKroist st = fst(chooseMoveFrom(makeMoveSet st))

--only works if AI is player 1
calcHeur :: GameState -> Int
calcHeur st = fst (countPieces st) - snd (countPieces st) + valueBoard st

valueBoard :: GameState -> Int
valueBoard st = 0
  where b = board st

--dealing with maybe States
testMove :: Move -> GameState -> Int
testMove m st = calcHeur (appMove m st)
  where tempState = st
        appMove :: Move -> GameState -> GameState
        --really dodgey here
        appMove m st
          | applyMove m st /= Nothing = applyMove m st
          | i == 0 = st
            where
              i = 1


makeMoveSet :: GameState -> [(Move, Int)]
makeMoveSet st = zip ml vl
  where ml = legalMoves st
        vl = func ml
          where func :: [Move] -> [Int]
                func [] = []
                func (x:xs) = testMove x st : func xs

chooseMoveFro :: [(Move, Int)] -> Move
chooseMoveFro l = fst((!!) l index 0 (intL l))
  where intL :: [(Move,Int)] -> [Int]
        intL (x:xs) = snd x : intL xs
        index :: Int -> [Int] -> Int


chooseMoveFrom :: [(Move, Int)] -> (Move, Int)
chooseMoveFrom (x:xs)
  | snd(x) > choseMoveFrom xs = x
  | otherwise = choseMoveFrom xs

{- version 2
--greedy st = head (moveSelect (head (evalMoves (legalMoves st) st)) (legalMoves st))
--greedy TroubleShooting
testLoc :: Location
testLoc = Location 1 1

testLoc2 :: Location
testLoc2 = Location 1 2

testMove :: Move
testMove = Move testLoc testLoc

testState :: GameState
testState = initialState (9,9)

testTurn :: Turn
testTurn = Turn Player2

moveList = [testMove, testMove, testMove]

--greedy

moveSelect :: Int -> [Move] -> [Move]
moveSelect _ [] = []
moveSelect 1 (x:xs) = x:[]
moveSelect n (x:xs) = moveSelect (n-1) xs

evalMoves :: [Move] -> GameState -> [Int]
evalMoves [] _ = []
evalMoves (x:xs) st = evalMove x st: evalMoves xs st
evalMoves _ _ = [-1]

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
adjLoc loc (x:xs) = head(chebList loc x): adjLoc loc xs

chebList :: Location -> Location -> [Location]
chebList l1 l2 = case (chebyshev l1 l2) of
  1 -> [l2]
  _ -> []


--gives Int list representing squares from locations
readLocations :: [Location] -> GameState -> [Int]
readLocations [] _ = []
readLocations (x:xs) st = readSFL x st :readLocations xs st

readSFL :: Location -> GameState -> Int
readSFL l st = readSquare (locSquare st l)

readSquare :: Square -> Int
readSquare sq = case sq of
  Piece Player2 -> 1
  Piece Player1 -> 2
  Empty -> 2
  Block -> 3
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
getL1 :: Move -> Location
getL1 (Move l _) = l
getL2 :: Move -> Location
getL2 (Move _ l) = l

getX :: Location -> Int
getX (Location x _) = x
getY :: Location -> Int
getY (Location _ y) = y
-}

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

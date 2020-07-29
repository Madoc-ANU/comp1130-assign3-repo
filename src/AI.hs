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
ais = [ ("firstLegalMove", NoLookahead firstLegalMove), ("basic", NoLookahead basic),
        ("default", WithLookahead defaultAI), ("wenKroist", NoLookahead wenKroist),
        ("queen", WithLookahead queen), ("king", Withlookahead king)
      ]

firstLegalMove :: GameState -> Move
firstLegalMove st = fst(legalMoves st)

wenKroist :: GameState -> Move
wenKroist st = fst (chooseMoveFrom (makeMoveSet st))

queen :: GameState -> Int -> Move
queen st i = fst (chooseMoveFrom (lookAheadSet st i (makeMoveSet st)))

king :: GameState -> Int -> Move
king st i = fst (chooseMoveFrom (recurLookAhead st i (makeMoveSet st)))

basic :: GameState -> Move
basic st = fst(minMaxMove st 0)

defaultAI :: GameState -> Int -> Move
defaultAI st depth = fst(head(minMaxMove st depth))

calcHeur :: GameState -> Int
calcHeur st
  | turn st == Turn Player1 = snd (countPieces st) - fst (countPieces st) + valueBoard st 9
  | turn st == Turn Player2 = fst (countPieces st) - snd (countPieces st) + valueBoard st 9
  | otherwise = 0
  where
    valueBoard :: GameState -> Int -> Int
    valueBoard _ _ = 0
    {-}
    valueBoard st
      |
      where b = board st
-}
testMove :: Move -> GameState -> Int
testMove m st = calcHeur (appMove m tempState)
-- | calcHeur
  where tempState = st
        appMove :: Move -> GameState -> GameState
        appMove m st = makeCertain(applyMove m st)

makeCertain :: Maybe GameState -> GameState
makeCertain st = case st of
  Nothing -> initialState (9,9)
  Just st -> st

makeMoveSet :: GameState -> [(Move, Int)]
makeMoveSet st = zip ml vl
  where ml = legalMoves st
        vl = func ml
          where func :: [Move] -> [Int]
                func [] = []
                func (x:xs) = testMove x st : func xs

--incomplete
chooseMoveFrom :: [(Move, Int)] -> (Move, Int)
chooseMoveFrom (a:[]) = a
chooseMoveFrom ((a,b):xs)
  | snd (chooseMoveFrom xs) > b = chooseMoveFrom xs
  | otherwise = (a,b)

--lookAheadFunctions
recurLookAhead :: GameState -> Int -> [(Move, Int)] -> [(Move, Int)]
recurLookAhead _ _ (a:[]) = a:[]
recurLookAhead st 0 l = l
recurLookAhead st i ((a,b):xs) = recurLookAhead st (i-1) (testPath st (a,b) : recurLookAhead st i xs)
  where
    enemyMoveValue :: GameState -> Int
    enemyMoveValue st = snd (chooseMoveFrom (recurLookAhead st (i-1) (makeMoveSet st)))
    testPath :: GameState -> (Move,Int) -> (Move,Int)
    testPath st (a,b) = (a, (b - enemyMoveValue (makeCertain (applyMove a tempState))))
      where
        tempState = st


lookAheadSet :: GameState -> Int -> [(Move, Int)] -> [(Move, Int)]
lookAheadSet _ _ (a:[]) = a:[]
lookAheadSet st 0 l = l
lookAheadSet st _ ((a,b):xs) = testPath st (a,b) : lookAheadSet st 1 xs
  where
    enemyMoveValue :: GameState -> Int
    enemyMoveValue st = snd (chooseMoveFrom (makeMoveSet st))
    testPath :: GameState -> (Move,Int) -> (Move,Int)
    testPath st (a,b) = (a, (b - enemyMoveValue (makeCertain (applyMove a tempState))))
      where
        tempState = st





-----------------------------


--defaultSupport
makeCertain :: Maybe GameState -> GameState
makeCertain st = case st of
  Nothing -> initialState (9,9)
  Just st -> st

makeMoveSet :: GameState -> [(Move, Int)]
makeMoveSet st = zip ml vl
  where ml = legalMoves st
        vl = func ml
          where func :: [Move] -> [Int]
                func [] = []
                func (x:xs) = testMove x st : func xs

makeStateSet :: [(Move,Int)] -> [GameState]
makeStateSet (x:[]) = x
makeStateSet ((a,b):xs)= makeCertain (applyMove a) : makeStateSet xs

basic :: GameState -> Int -> [(Move,Int)] -> [(Move, Int)]
basic st 0 (x:xs) = chooseMoveFrom(st):
basic st depth l =
  where
    moves = makeMoveSet st
    states = makeStateSet moves

--Player 1 is positive
heur :: GameState -> Int
heur st = fst (countPieces st) - snd (countPieces st)

testMove :: Move -> GameState -> Int
testMove m st = heur (appMove m tempState)
  where tempState = st
        appMove :: Move -> GameState -> GameState
        appMove m st = makeCertain(applyMove m st)


--incomplete + could be merged
maxMove :: [(Move, Int)] -> (Move, Int)
maxMove (a:[]) = a
maxMove ((a,b):xs)
  | snd (maxMove xs) > b = maxMove xs
  | otherwise = (a,b)

minMove :: [(Move, Int)] -> (Move, Int)
minMove (a:[]) = a
minMove ((a,b):xs)
  | snd (minMove xs) < b = minMove xs
  | otherwise = (a,b)

minMaxMove :: GameState -> Int -> (Move,Int)
minMaxMove st 0
  | turn st == Turn Player1 = (maxMove (makeMoveSet st))
  | turn st == Turn Player2 = (minMove (makeMoveSet st))
minMaxMove st depth
  | turn st == Turn Player1 = minMaxMove newState (depth-1)
  | turn st == Turn Player2 = minMaxMove newState (depth-1)
    where
      stepState = applyMove fst (minMaxMove st 0)
      newState = flipPlayer st
      flipPlayer :: GameState -> GameState
      flipPlayer st = State (otherPlayer (turn st)) (bounds st) (board st) (history st)
        where
          history :: GameState -> [Board]
          history (State _ _ _ h) = h


--incomplete
chooseMoveFrom :: [(Move, Int)] -> (Move, Int)
chooseMoveFrom (a:[]) = a
chooseMoveFrom ((a,b):xs)
  | snd (chooseMoveFrom xs) > b = chooseMoveFrom xs
  | otherwise = (a,b)

--lookAheadFunctions
recurLookAhead :: GameState -> Int -> [(Move, Int)] -> [(Move, Int)]
recurLookAhead _ _ (a:[]) = a:[]
recurLookAhead st 0 l = l
recurLookAhead st i ((a,b):xs) = recurLookAhead st (i-1) (testPath st (a,b) : recurLookAhead st i xs)
  where
    enemyMoveValue :: GameState -> Int
    enemyMoveValue st = snd (chooseMoveFrom (recurLookAhead st (i-1) (makeMoveSet st)))
    testPath :: GameState -> (Move,Int) -> (Move,Int)
    testPath st (a,b) = (a, (b - enemyMoveValue (makeCertain (applyMove a tempState))))
      where
        tempState = st


lookAheadSet :: GameState -> Int -> [(Move, Int)] -> [(Move, Int)]
lookAheadSet _ _ (a:[]) = a:[]
lookAheadSet st 0 l = l
lookAheadSet st _ ((a,b):xs) = testPath st (a,b) : lookAheadSet st 1 xs
  where
    enemyMoveValue :: GameState -> Int
    enemyMoveValue st = snd (chooseMoveFrom (makeMoveSet st))
    testPath :: GameState -> (Move,Int) -> (Move,Int)
    testPath st (a,b) = (a, (b - enemyMoveValue (makeCertain (applyMove a tempState))))
      where
        tempState = st



{-}

minMax :: GameState -> Int -> (Move, Int)
minMax st 0 = chooseMoveFrom(genMoveSet st)
minMax st 1 = recur (genMoveSet st)
  where
    recur :: -> (Move, Int)

calcHeur :: GameState -> Int
calcHeur st
  | turn st == Turn Player1 = fst (countPieces st) - snd (countPieces st) + valueBoard st 9
  | turn st == Turn Player2 = snd (countPieces st) - fst (countPieces st) + valueBoard st 9
  | otherwise = 0
  where
    valueBoard :: GameState -> Int -> Int
    valueBoard _ _ = 0



--lookAheadFunctions
lookAheadSet :: GameState -> Int -> [(Move, Int)] -> [(Move, Int)]
lookAheadSet _ _ (a:[]) = a:[]
lookAheadSet st 0 l = l
lookAheadSet st _ ((a,b):xs) = testPath st (a,b) : lookAheadSet st 1 xs
  where
    enemyMoveValue :: GameState -> Int
    enemyMoveValue st = snd (chooseMoveFrom (makeMoveSet st))
    testPath :: GameState -> (Move,Int) -> (Move,Int)
    testPath st (a,b) = (a, (b - enemyMoveValue (makeCertain (applyMove a tempState))))
      where
        tempState = st

--MinMaxFunctions
recurLookAhead :: GameState -> Int -> [(Move, Int)] -> [(Move, Int)]
recurLookAhead _ _ (a:[]) = a:[]
recurLookAhead st 0 l = l
recurLookAhead st i ((a,b):xs) = recurLookAhead st (i-1) (testPath st (a,b) : recurLookAhead st i xs)
  where
    enemyMoveValue :: GameState -> Int
    enemyMoveValue st = snd (chooseMoveFrom (recurLookAhead st (i-1) (makeMoveSet st)))
    testPath :: GameState -> (Move,Int) -> (Move,Int)
    testPath st (a,b) = (a, (b - enemyMoveValue (makeCertain (applyMove a tempState))))
      where
        tempState = st

-}
{-

{-
protoType :: GameState -> Move
protoType st = head (moveSelect 10 (legalMoves st))

greedy :: GameState -> Move
greedy st = head (moveSelect (head(evalMoves (legalMoves st) st)) (legalMoves st))
-}

chooseMoveFrom3 :: [(Move, Int)] -> (Move, Int)
chooseMoveFrom3 (x:xs) = (!!) index (x:xs)
  where
    vl = pullValues tl
    pullValues :: [(Move, Int)] -> [Int]
    pullValues (q:qs) = snd q : pullValues qs
    index = getIndex vl
      where
        getIndex :: [Int] -> Int -> Int
        getIndex ref (i:is)
          | genIndex is > i = getIndex is
          | otherwise = i


-}



{-}

chooseMoveFrom3 :: [(Move, Int)] -> (Move, Int)
chooseMoveFrom3 (x:xs) = (!!) index (x:xs)
  where
    ml = pullMoves tl
    vl = pullValues tl
    pullMoves :: [(Move, Int)] -> [Move]
    pullMoves (n:ns) = fst n : pullMoves ns
    pullValues :: [(Move, Int)] -> [Int]
    pullValues (q:qs) = snd q : pullValues qs
    index = getIndex vl
      where
        getIndex :: [Int] -> Int -> Int
        getIndex (x:xs)
          | getLargest (x:xs) == x =
          | otherwise
          where
            getLargest :: [Int] -> [Int]
            getLargest (i:is)
              | genIndex is > i = getIndex is
              | otherwise = i




  chooseMoveFrom :: [(Move, Int)] -> (Move, Int)
  chooseMoveFrom (x:xs) == (!!) index (x:xs)
    where index = getIndex
          getIndex




chooseMoveFrom :: [(Move, Int)] -> (Move, Int)
chooseMoveFrom (x:xs)
  where getLargest :: [(Move, Int)] -> Int
        getLargest


-}
tMove :: Move
tMove = Move testLoc testLoc


testLoc :: Location
testLoc = Location 1 1

testLoc2 :: Location
testLoc2 = Location 1 2



testState :: GameState
testState = initialState (9,9)

testTurn :: Turn
testTurn = Turn Player2

moveList = [testMove, testMove, testMove]




{- version 2
--greedy st = head (moveSelect (head (evalMoves (legalMoves st) st)) (legalMoves st))
--greedy TroubleShooting


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

--BACKUP


{-|
Module      : AI
Description : AIs for Ataxx
Copyright   : (c) 2020 Madoc Thomas Cottle
License     : AllRightsReserved
-}

{-}
module AI where

import Ataxx

data AIFunc
  = NoLookahead (GameState -> Move)
  | WithLookahead (GameState -> Int -> Move)
ais :: [(String, AIFunc)]
ais = [ ("firstLegalMove", NoLookahead firstLegalMove), ("wenKroist", NoLookahead wenKroist), ("queen", WithLookahead queen), ("king", WithLookahead king)
      ]

firstLegalMove :: GameState -> Move
firstLegalMove st = head (legalMoves st)

wenKroist :: GameState -> Move
wenKroist st = fst (chooseMoveFrom (makeMoveSet st))

queen :: GameState -> Int -> Move
queen st i = fst (chooseMoveFrom (lookAheadSet st i (makeMoveSet st)))

king :: GameState -> Int -> Move
king st i = fst (chooseMoveFrom (recurLookAhead st i (makeMoveSet st)))

calcHeur :: GameState -> Int
calcHeur st
  | turn st == Turn Player1 = snd (countPieces st) - fst (countPieces st) + valueBoard st 9
  | turn st == Turn Player2 = fst (countPieces st) - snd (countPieces st) + valueBoard st 9
  | otherwise = 0
  where
    valueBoard :: GameState -> Int -> Int
    valueBoard _ _ = 0
    {-}
    valueBoard st
      |
      where b = board st
-}
testMove :: Move -> GameState -> Int
testMove m st = calcHeur (appMove m tempState)
-- | calcHeur
  where tempState = st
        appMove :: Move -> GameState -> GameState
        appMove m st = makeCertain(applyMove m st)

makeCertain :: Maybe GameState -> GameState
makeCertain st = case st of
  Nothing -> initialState (9,9)
  Just st -> st

makeMoveSet :: GameState -> [(Move, Int)]
makeMoveSet st = zip ml vl
  where ml = legalMoves st
        vl = func ml
          where func :: [Move] -> [Int]
                func [] = []
                func (x:xs) = testMove x st : func xs


-}

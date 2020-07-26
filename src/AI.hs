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
protoType st = head (moveSelect 3 (legalMoves st))

-- | A very simple AI, which picks the first move returned by the
-- 'legalMoves' function. AIs can rely on the 'legalMoves' list being
-- non-empty; if there were no legal moves, the framework would have
-- ended the game.

caclHeur :: GameState -> Int
caclHeur st = fst (countPieces st) - snd (countPieces st) + (squareValue st)

sumList :: [a]

squareValue :: GameState -> Int
squareValue st = foldl' count (0,0) (concat (board st))
  where
    count :: (Int, Int) -> Square -> (Int, Int)
    count (p1,p2) sq = case sq of
        Piece Player1 ->

squareValue ::
--minMax :: Int -> [Move] -> Int
--minMax acc (x:xs) =

moveValue :: Move -> Int
moveValue m = 1

moveSelect :: Integer -> [Move] -> [Move]
moveSelect _ [] = []
moveSelect 1 (x:xs) = x:[]
moveSelect n (x:xs) = moveSelect (n-1) xs

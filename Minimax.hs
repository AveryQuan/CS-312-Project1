-- CPSC 312 - 2023 - Games in Haskell
module Minimax where

-- To run it, try:
-- ghci
-- :load Minimax
import Checkers
--import MagicSum
-- import CountGame

maxDepth = 3

----   Determining the best move  ---
minimax:: Game -> State -> Int -> (Action, Double)
-- minimax game state   =>  (move,value_to_player)

-- precondition: there are some moves that are available
minimax game state depth =
      let (State _ actions) = state
      in
      argmax (valueact game state (depth-1)) actions


-- valueact game st action  is the value of doing action act in state st for game
valueact:: Game -> State -> Int -> Action  -> Double
--valueact game st depth act = value game (game act st) depth
valueact game st depth act = value game (game act st) depth
--    | depth == 0 = evaluateBoard st
--    | otherwise = value game (game act st) depth


-- value minimax game result  = value  for current player after result
value:: Game -> Result -> Int -> Double
value _  (EndOfGame val _) depth = val*100
--value game (ContinueGame st) depth = - snd (minimax game st (depth+1))
value game (ContinueGame st) depth
    | depth == 0 = evaluateBoard st
    | otherwise =  - snd (minimax game st (depth))   -- (action,value) for next player
                                                   -- value for current player is negative of value of the other player

-- return ratio of num player pieces / opponent pieces.
evaluateBoard:: State -> Double
evaluateBoard (State (board, player) _) = getPiecesValue board (getOpponentNumber player) - (getPiecesValue board player)

getPiecesValue:: Board -> ActivePlayer -> Double
getPiecesValue board player = foldr (\ col val -> (getNumKings col player) + (getNumNormalPieces col player) + val)  0 board

-- Kings have value of 4
getNumKings:: [Int] -> ActivePlayer -> Double
getNumKings col player = fromIntegral (length (filter (\piece -> piece == (player+2)) col) * 4)

-- Pieces have value of 1
getNumNormalPieces:: [Int] -> ActivePlayer -> Double
getNumNormalPieces col player = fromIntegral (length (filter (\piece -> piece == player) col))

getOpponentNumber:: Int -> Int
getOpponentNumber player
    | player == whitePlayer = blackPlayer
    | player == blackPlayer = whitePlayer


-- to find the best opening move
-- minimax magicsum magicsum_start

-- show timings for evaluation:
-- :set +s

--Try (for Checkers)
-- minimax checkers checkers_start 2
-- minimax checkers simple_start 2


mm_player:: Game -> Player
mm_player game state = fst ( minimax game state maxDepth)


-- argmax f lst  = (e, f e) for e <- lsts where f e is maximal
-- Precondition: lst is not empty
--  Note that this does not require the elements of lst to be comparable, only value
-- like  max[(e,f e) <- e in lst] but where only the second elements of pairs are compared in the max.
argmax :: Ord v => (e -> v) -> [e] -> (e,v)
argmax f [e] = (e, f e)
argmax f (h:t)
   | fh > ft = (h,fh)
   | otherwise = (bt, ft)
   where
      (bt,ft) = argmax f t
      fh = f h

--- after surfing the web, I found this is the standard argmaxWithMax
--  http://hackage.haskell.org/package/list-extras-0.4.1.4/docs/Data-List-Extras-Argmax.html

-- Test case:
-- argmax (\x -> 5- (x-2)^2) [0..10]
-- argmax (\x -> 1 + 4*x - x^2) [0..10]

-- another implementation
argmax2 :: Ord v => (e -> v) -> [e] -> (e,v)
argmax2 f (h:t) =
   foldr (\ e (et,vt) -> let fe = f e in
                         if (fe > vt) then (e,fe) else (et,vt))
         (h, f h) t

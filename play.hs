-- CPSC 312 - 2023 - Games in Haskell
module Play where

-- To run it, try:
-- ghci
-- :load Play

import Checkers

import Minimax  -- make sure same game is imported in Minimax

import IOHelpers

import System.IO
import Text.Read   (readMaybe)

type TournammentState = (Int,Int,Int)   -- wins, losses, ties


play :: Game -> State -> Player -> TournammentState -> IO TournammentState

play game start_state opponent ts =
  let (wins, losses,ties) = ts in
  do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      line <- getLineFixed
      if line == "0"
        then
            person_play game (ContinueGame start_state) opponent ts
        else if line ==  "1"
             then computer_play game (ContinueGame start_state) opponent ts
        else if line == "2"
            then return ts
        else play game start_state opponent ts

person_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- opponent has played, the person must now play

person_play game (ContinueGame state) opponent ts =
   do
      let State internal avail = state
      let (board, _) = internal
      printBoard board
      putStrLn ("Available moves: " ++ show avail)
      line <- getLineFixed
      case (readMaybe line :: Maybe Action) of
        Nothing ->
           person_play game (ContinueGame state) opponent ts
        Just action ->
           if (action `elem` avail)
             then
                computer_play game (game action state) opponent ts
             else
               do
                putStrLn ("Illegal move: "++ show action)
                person_play game (ContinueGame state) opponent ts

person_play game (EndOfGame val start_state) opponent ts =
  do
    newts <- update_tournament_state (-val) ts  -- val is value to computer; -val is value for person
    play game start_state opponent newts

computer_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- computer_play game current_result opponent ts
-- person has played, the computer must now play
computer_play game (EndOfGame val start_state) opponent ts =
   do
      newts <- update_tournament_state val ts
      play game start_state opponent newts

computer_play game (ContinueGame state) opponent ts =
      let
          opponent_move = opponent state
        in
          do
            putStrLn ("The computer chose "++show opponent_move)
            person_play game (game opponent_move state) opponent ts

update_tournament_state:: Double -> TournammentState -> IO TournammentState
-- given value to the person, the tournament state, return the new tournament state
update_tournament_state val (wins,losses,ties)
  | val > 0 = do
      putStrLn "You Won"
      return (wins+1,losses,ties)
  | val == 0 = do
      putStrLn "It's a tie"
      return (wins,losses,ties+1)
  | otherwise = do
      putStrLn "Computer won!"
      return (wins,losses+1,ties)

-- If you imported Checkers try:
-- play checkers checkers_start simple_player (0,0,0)
-- play checkers checkers_start (mm_player checkers) (0,0,0)

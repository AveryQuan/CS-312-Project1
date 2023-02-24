-- CPSC 312 - 2023 - Games in Haskell
module Checkers where

-- To run it, try:
-- ghci
-- :load Checkers

data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

------ Checkers -------

-- TODO: redefine Action
data Action = Action Int                   -- a move for a player is just an Int
         deriving (Ord,Eq)
-- TODO: redefine InternalState
type InternalState = ([Action],[Action])   -- (self,other)

initialRow = [0,2,0,0,0,1,0,1]
reverseinitialRow = [2,0,2,0,0,0,1,0]

initialState :: Board
initialState = [initialRow, reverseinitialRow,initialRow,reverseinitialRow,initialRow,reverseinitialRow,initialRow,reverseinitialRow]

getNormalMoves state color = getNormalMovesHelp state 0 0 color -- color 2 for red, 1 for black.

getNormalMovesHelp _ 8 _ _= []
getNormalMovesHelp state row col color -- When iterating through 
    | col == 7 = if (state !! row !! col /= color && state !! row !! col /= (color+2))  then getNormalMovesHelp state (row+1) 0 color else getNormalMovesSquare state row col ++ getNormalMovesHelp state (row+1) 0 color
    | otherwise = if (state !! row !! col /= color && state !! row !! col /= (color+2))  then getNormalMovesHelp state row (col+1) color else getNormalMovesSquare state row col ++ getNormalMovesHelp state row (col+1) color                

-- Gets all possible normal moves for a square by first checking its content.
getNormalMovesSquare [] _ _ = []
getNormalMovesSquare state row col 
    | state !! row !! col == 0 = [] --Empty square, no move
    | state !! row !! col == 1 = getDirectionalMovesSquare state row col "bLeft" ++ getDirectionalMovesSquare state row col "bRight"  -- This is the left and right of a black piece going from the right side of the board to the left.
    | state !! row !! col == 2 = getDirectionalMovesSquare state row col "left" ++ getDirectionalMovesSquare state row col "right"   -- This is the left and right of a red piece going from the left side of the board to the right.
    | state !! row !! col >= 3 = getDirectionalMovesSquare state row col "bLeft" ++ getDirectionalMovesSquare state row col "bRight" ++ getDirectionalMovesSquare state row col "left" ++ getDirectionalMovesSquare state row col "right"  -- Kings move in 1 of 4 diag

getDirectionalMovesSquare state row col dir 
    | state !! row !! col == 0 = [] -- Empty
    | dir == "left" = if (row == 0 || col == 7 || (state !! (row-1) !! (col+1)) /= 0) then [] else [[(row,col),(row-1,col+1)]] -- Can't move left diagonally if on topmost row or on rightmost column
    | dir == "right" = if (row == 7 || col == 7 || (state !! (row+1) !! (col+1)) /= 0) then [] else [[(row,col),(row+1,col+1)]] -- Can't move right diagonally if on topmost row or on rightmost column
    | dir == "bLeft" = if (col == 0 || row == 0 || (state !! (row-1) !! (col-1)) /= 0) then [] else [[(row,col),(row-1,col-1)]]
    | dir == "bRight" = if (col == 0 || row == 7 || (state !! (row+1) !! (col-1)) /= 0) then [] else [[(row,col),(row+1,col-1)]] 


-- magicsum :: Game
-- magicsum move (State (mine,others) available) 
--     | win move mine                = EndOfGame 1  magicsum_start     -- agent wins
--     | available == [move]          = EndOfGame 0  magicsum_start     -- no more moves, tie
--     | otherwise                    =
--           ContinueGame (State (others,(move:mine))   -- note roles have flipped
--                         [act | act <- available, act /= move])

-- TODO: redefine win
-- win n ns = the agent wins if it selects n given it has already selected ns
-- win :: Action -> [Action] -> Bool
-- win (Action n) ns  = or [n+x+y==15 | Action x <- ns, Action y <- ns, x/=y]

-- magicsum_start = State ([],[]) [Action n | n <- [1..9]]

-- TODO: redefine Show
-- show and read actions just as the integer
instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]

------- A Player -------

-- TODO: player that picks a random move from list of all available moves
-- prereq: function to generate all available moves given State
-- simple_player :: Player
-- this player has an ordering of the moves, and chooses the first one available
-- simple_player (State _ avail) = head [Action e | e <- [5,6,4,2,8,1,3,7,9],
--                                                Action e `elem` avail]


-- Test cases
-- magicsum (simple_player magicsum_start) magicsum_start
-- a i = Action i  -- make it easier to type
-- as lst = [Action i | i <- lst]
-- magicsum (a 6) (State (as [3,5], as [2,7]) (as [1,4,6,8,9])) 
-- magicsum (a 3) (State (as [5,7], as [2,9]) (as [1,3,4,6,8])) 


------- temporary -------
type Board = [[Int]]

printBoard :: Board -> IO()
printBoard board = do
    let a = [formattedRow (board !! y) y | y <- [idxs-1,idxs-2..0]]
    let linePadding = unwords ("   " : ["---" | x <-[0..idxs-1]])
    let b = concat (map (\x -> [x] ++ [linePadding]) a)
    putStr (unlines (linePadding : b))
    putStr ("    ") -- padding
    putStrLn (unwords [" " ++ show x ++ " " | x <- [0..idxs-1]])
      where idxs = length board

formattedRow :: Show a => [Int] -> a -> String
formattedRow row idx = 
    show idx ++ "  " ++ (printRow row) ++ " |"
      where printRow row = unwords ["| " ++ (convertValueToPiece x) | x <- row]

convertValueToPiece :: Int -> String
convertValueToPiece value
   | value == 1   = "b"
   | value == 2   = "w"
   | value == 3   = "B"
   | value == 4   = "W"
   | otherwise    = " "

-- testBoards:
testBoard :: [[Int]]
testBoard = [[0, 1, 0, 0, 0, 2, 0, 0],
            [1, 0, 0, 0, 2, 0, 0, 0],
            [0, 0, 0, 1, 0, 2, 0, 2],
            [1, 0, 1, 0, 0, 0, 0, 0],
            [0, 1, 0, 1, 0, 2, 0, 2],
            [1, 0, 1, 0, 2, 0, 2, 0],
            [0, 0, 0, 0, 0, 2, 0, 2],
            [0, 0, 1, 0, 0, 0, 0, 0]]
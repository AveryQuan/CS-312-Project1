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
data Action = Action [(Int,Int)]                -- a move for a player is a sequence of pairs
         deriving (Ord,Eq)
type Board = [[Int]]
type InternalState = (Board, ActivePlayer)      -- state of the board and current colour
type ActivePlayer = Int                         -- 1 = black; 2 = red/white 

----  Initialization ----
initialRow = [0,2,0,0,0,1,0,1]
reverseinitialRow = [2,0,2,0,0,0,1,0]

initialBoard :: Board
initialBoard = [initialRow,reverseinitialRow,initialRow,reverseinitialRow,initialRow,reverseinitialRow,initialRow,reverseinitialRow]

blackPlayer = 1
whitePlayer = 2

initialState = (initialBoard, blackPlayer)

---- Move Generation ----
getAvailableMoves:: InternalState -> [Action] 
getAvailableMoves state =
    let (board, activePlayer) = state
    in
    if getJumpMoves board activePlayer == []
        then getNormalMoves board activePlayer
    else getJumpMoves board activePlayer

getNormalMoves :: Board -> ActivePlayer -> [Action]
getNormalMoves state color = getNormalMovesHelp state 0 0 color -- color 2 for white/red, 1 for black.

getNormalMovesHelp :: Board -> Int -> Int -> ActivePlayer -> [Action]
getNormalMovesHelp _ 8 _ _= []
getNormalMovesHelp state row col color -- When iterating through 
    | col == 7 = if (state !! row !! col /= color && state !! row !! col /= (color+2))  
        then getNormalMovesHelp state (row+1) 0 color 
        else getNormalMovesSquare state row col ++ getNormalMovesHelp state (row+1) 0 color
    | otherwise = if (state !! row !! col /= color && state !! row !! col /= (color+2))  
        then getNormalMovesHelp state row (col+1) color 
        else getNormalMovesSquare state row col ++ getNormalMovesHelp state row (col+1) color                

-- Gets all possible normal moves for a square by first checking its content.
getNormalMovesSquare :: Board -> Int -> Int -> [Action]
getNormalMovesSquare [] _ _ = []
getNormalMovesSquare state row col 
    | state !! row !! col == 0 = [] --Empty square, no move
    | state !! row !! col == 1 = getDirectionalMovesSquare state row col "bLeft" ++ getDirectionalMovesSquare state row col "bRight"  -- This is the left and right of a black piece going from the right side of the board to the left.
    | state !! row !! col == 2 = getDirectionalMovesSquare state row col "left" ++ getDirectionalMovesSquare state row col "right"   -- This is the left and right of a red piece going from the left side of the board to the right.
    | state !! row !! col >= 3 = getDirectionalMovesSquare state row col "bLeft" ++ getDirectionalMovesSquare state row col "bRight" ++ getDirectionalMovesSquare state row col "left" ++ getDirectionalMovesSquare state row col "right"  -- Kings move in 1 of 4 diag

getDirectionalMovesSquare :: Board -> Int -> Int -> String -> [Action]
getDirectionalMovesSquare state row col dir 
    | state !! row !! col == 0 = [] -- Empty
    | dir == "left" = if (row == 7 || col == 7 || (state !! (row+1) !! (col+1)) /= 0) then [] else [Action [(row,col),(row+1,col+1)]] -- Can't move left diagonally if on topmost row or on rightmost column
    | dir == "right" = if (row == 0 || col == 7 || (state !! (row-1) !! (col+1)) /= 0) then [] else [Action [(row,col),(row-1,col+1)]] -- Can't move right diagonally if on bottom row or on rightmost column
    | dir == "bLeft" = if (col == 0 || row == 0 || (state !! (row-1) !! (col-1)) /= 0) then [] else [Action [(row,col),(row-1,col-1)]]
    | dir == "bRight" = if (col == 0 || row == 7 || (state !! (row+1) !! (col-1)) /= 0) then [] else [Action [(row,col),(row+1,col-1)]] 


-- Make this function work with all dirs somehow?
getDirectionalJumpsSquare :: Board -> Int -> Int -> String -> [Action]
getDirectionalJumpsSquare state row col dir 
    | state !! row !! col == 0 = [] -- Empty
    | dir == "left" = if (row >= 6 || col >= 6 || (state !! (row+2) !! (col+2)) /= 0 || (jumpable state (row+1) (col+1) (state !! row !! col)) == False) then [] else [Action [(row,col),(row+2,col+2)]] -- Can't move left diagonally if on top row or on rightmost column
    | dir == "right" = if (row <= 1 || col >= 6 || (state !! (row-2) !! (col+2)) /= 0 || (jumpable state (row-1) (col+1) (state !! row !! col)) == False) then [] else [Action [(row,col),(row-2,col+2)]] -- Can't move right diagonally if on bottom row or on rightmost column
    | dir == "bLeft" = if (col <= 1 || row <= 1 || (state !! (row-2) !! (col-2)) /= 0 || (jumpable state (row-1) (col-1) (state !! row !! col)) == False) then [] else [Action [(row,col),(row-2,col-2)]]
    | dir == "bRight" = if (col <= 1 || row >= 6 || (state !! (row+2) !! (col-2)) /= 0 || (jumpable state (row+1) (col-1) (state !! row !! col)) == False) then [] else [Action [(row,col),(row+2,col-2)]] 

jumpable :: Board -> Int -> Int -> ActivePlayer -> Bool
jumpable state row col color = (state !! row !! col /= 0) && (((state !! row !! col ) `mod` 2) /= (color `mod` 2)) -- this is the square we want to jump over, must be different color and non-empty.

getJumpMoves :: Board -> ActivePlayer -> [Action]
getJumpMoves state color = getJumpMovesHelp state 0 0 color -- color 2 for red, 1 for black.

getJumpMovesHelp :: Board -> Int -> Int -> ActivePlayer -> [Action]
getJumpMovesHelp _ 8 _ _= []
getJumpMovesHelp state row col color -- When iterating through 
    | col == 7 = if (state !! row !! col /= color && state !! row !! col /= (color+2))  then getJumpMovesHelp state (row+1) 0 color else getJumpMovesSquare state row col ++ getJumpMovesHelp state (row+1) 0 color
    | otherwise = if (state !! row !! col /= color && state !! row !! col /= (color+2))  then getJumpMovesHelp state row (col+1) color else getJumpMovesSquare state row col ++ getJumpMovesHelp state row (col+1) color      

getJumpMovesSquare :: Board -> Int -> Int -> [Action]
getJumpMovesSquare [] _ _ = []
getJumpMovesSquare state row col 
    | state !! row !! col == 0 = [] --Empty square, no move
    | state !! row !! col == 1 = getDirectionalJumpsSquare state row col "bLeft" ++ getDirectionalJumpsSquare state row col "bRight"  -- This is the left and right of a black piece going from the right side of the board to the left.
    | state !! row !! col == 2 = getDirectionalJumpsSquare state row col "left" ++ getDirectionalJumpsSquare state row col "right"   -- This is the left and right of a red piece going from the left side of the board to the right.
    | state !! row !! col >= 3 = getDirectionalJumpsSquare state row col "bLeft" ++ getDirectionalJumpsSquare state row col "bRight" ++ getDirectionalJumpsSquare state row col "left" ++ getDirectionalJumpsSquare state row col "right" 

---- State Update ----
-- This assigns a value to the element in (row,col) in state.
assign :: Board -> Int -> Int -> Int -> Board
assign state row col value 
    | value < 0 || value > 4 = state -- May only assign valid values f in [1,4] 
    | otherwise = fst(splitAt row state) ++ [fst(splitAt col (state !! row)) ++ [value] ++ snd(splitAt (col+1) (state !! row))] ++ snd(splitAt (row+1) state)

makeMove :: Board -> (Int, Int) -> (Int, Int) -> Board
makeMove state (o1,o2) (n1,n2) 
    | state !! n1 !! n2 /= 0 = state -- Do nothing.
    | otherwise = assign (assign state n1 n2 (state !! o1 !! o2)) o1 o2 0 --Make normal move.

makeJump :: Board -> (Int, Int) -> (Int, Int) -> Board
makeJump state (o1,o2) (n1,n2) 
    | state !! n1 !! n2 /= 0 = state -- Do nothing: Destination occupied.
    | state !! ((o1+n1) `div` 2) !! ((o2+n2) `div` 2) == 0 || ((state  !! ((o1+n1) `div` 2) !! ((o2+n2) `div` 2)) `mod` 2) == (state !! o1 !! o2 `mod` 2) = state -- Do nothing.
    | otherwise =  assign ( assign (assign state n1 n2 (state !! o1 !! o2)) ((o1+n1) `div` 2) ((o2+n2) `div` 2) 0 ) o1 o2 0 --Make normal move. assign state n1 n2 (state !! o1 !! o2) --

flipPlayer :: InternalState -> InternalState
flipPlayer state =
    let (board, activePlayer) = state
    in
        if (activePlayer == blackPlayer) 
            then (board, whitePlayer)
        else (board, blackPlayer)

-- TODO: given an action, update the current board (without flipping players)
updateState :: Action -> InternalState -> InternalState
updateState move state = state

-- magicsum :: Game
-- magicsum move (State (mine,others) available) 
--     | win move mine                = EndOfGame 1  magicsum_start     -- agent wins
--     | available == [move]          = EndOfGame 0  magicsum_start     -- no more moves, tie
--     | otherwise                    =
--           ContinueGame (State (others,(move:mine))   -- note roles have flipped
--                         [act | act <- available, act /= move])

checkers :: Game
checkers move (State internalState available)
    | win updatedState                  = EndOfGame 1 checkers_start
    | otherwise                         =
        ContinueGame (State (flipPlayer updatedState) (getAvailableMoves (flipPlayer updatedState)))
            where updatedState = (updateState move internalState)

-- if the opponent cannot make a move, then current player wins
win :: InternalState -> Bool
win state = (getAvailableMoves (flipPlayer state) == [])

-- reset
checkers_start = State initialState (getAvailableMoves initialState)

instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]

------- A Player -------

-- this player chooses the first generated move always
simple_player :: Player
simple_player (State _ avail) = head avail


-- Test cases
-- magicsum (simple_player magicsum_start) magicsum_start
-- a i = Action i  -- make it easier to type
-- as lst = [Action i | i <- lst]
-- magicsum (a 6) (State (as [3,5], as [2,7]) (as [1,4,6,8,9])) 
-- magicsum (a 3) (State (as [5,7], as [2,9]) (as [1,3,4,6,8])) 


------- temporary -------
-- type Board = [[Int]]

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
            [0, 0, 0, 1, 0, 1, 0, 2],
            [1, 0, 1, 0, 2, 0, 0, 0],
            [0, 1, 0, 1, 0, 1, 0, 2],
            [1, 0, 0, 0, 2, 0, 2, 0],
            [0, 0, 0, 0, 0, 2, 0, 2],
            [0, 0, 1, 0, 0, 0, 0, 0]]
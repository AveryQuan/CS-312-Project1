# CS-312-project
Checkers with AI opponent with varying difficulty
- AI opponent uses MinMax algorithm

Components to create:

1. checkers.hs
  - internal state (2d array)
  - result (have to print board to console)
  - action defines available actions
2. moves (determine possible moves)
3. play.hs
4. minimax.hs



Board:

use 1234 2d array
(0, 0) is bottom left corner
Odd numbers = black
Even numbers = white

0 = empty
1,2 = common pieces
3,4 = kings

Input: 

[(start), (jumps), (end)]

Output: 

print state of the board, only have to print one direction because only playing against ai

Methods:

Chris 
- getAvailableMoves()     
     - getJumpMoves()
     - getNormalMoves()

Avery
- initializeState()

Jordan
- checkWin()
- printBoard()



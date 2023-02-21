-- CPSC 312 - 2023 - Games in Haskell
module Games2 
  (State(..), Result(..), Game, Rands, Player,  mkRands) where
-- To load it, try:
-- ghci
-- :load Games2

-- you may have to do:     cabal install --lib random
import System.Random


-- gs=game_state  act=action 
data State gs act = State gs [act]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result gs act =
              EndOfGame String Double (State gs act)    -- end of game: message, value, starting state
            | ContinueGame Double (State gs act)        -- continue with reward and new state
         deriving (Eq, Show)

type Game gs act = act -> State gs act -> Result gs act

-- gs=game_state  act=action ps=player_state
type Player gs act ps = Game gs  act -> Result gs act -> ps -> (act, ps)

------
-- trand test random numbers
trand :: IO Bool
trand = do
  g <- newStdGen   -- use getStdGen to get same random number generator each time
  return (ace_first [min 10 ( 1+ (abs n) `mod` 13) | n <- (randoms g :: [Int])])

-- ace_first deck   is true if an Ace (1) comes before a picture card (10 - K)
ace_first :: (Eq a, Num a) => [a] -> Bool
ace_first (1:_) = True
ace_first (10:_) = False
ace_first (_:t) = ace_first t


-------- The following can be used to implement a deck in a game.
-- Put this in your game, and don't export a way to create or access RandsC.
-- for random numbers in states, do not allow print or comparisons to depend on them
-- Don't export access to the constructor. So no one else can look at them or construct them
-- i.e., export Rands, but not RandsC (see top of file)
data Rands = RandsC [Double]    -- random numbers. Do not print or compare them!!!
instance Show Rands where
   show _ = "_"
instance Eq Rands where
   x == y = True
instance Ord Rands where
   -- x < y = False
   x <= y = True

-- Make randoms (can be used in a do)
mkRands :: RandomGen g => g -> Rands
mkRands rg = RandsC (randoms rg)

test_mkRands :: IO Rands
test_mkRands =
  do 
    g1 <- newStdGen
    return (mkRands g1)
  

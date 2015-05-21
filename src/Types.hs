-- | ???
module Types where

-- | ???
data Player
  = White   -- ^ ???
  | Black   -- ^ ???
  | Empty   -- ^ ???

-- | ???
type Field = [[Player]]

-- | ???
data State = State{
	board   :: Field,       -- ^ ???
	player  :: Player,      -- ^ ???
	score   :: (Int, Int)   -- ^ ???
}

-- | ???
type Cell = (Int, Int)

instance Show Player where
	show White = "x"
	show Black = "o"
	show Empty = "_"

instance Eq Player where
	White == White = True
	Black == Black = True
	Empty == Empty = True
	_ == _ = False 

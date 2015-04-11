module Main where
import Data.Monoid

main :: IO ()
main = undefined
data Player = White | Black | Empty
type Field = [[Player]]
type Revert = Maybe [Field]
type Move = (Field, Player, Int, Int)

instance Show Player where
	show White = "x"
	show Black = "o"
	show Empty = "_"

instance Eq Player where
	White == White = True
	Black == Black = True
	Empty == Empty = True
	_ == _ = False 

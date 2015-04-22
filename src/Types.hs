{-# OPTIONS_GHC -Wall #-}
module Types where

--main :: IO ()
--main = undefined
data Player = White | Black | Empty
type Field = [[Player]]
type Move = (Field, Player, Int, Int)
type Revert = Maybe Move

instance Show Player where
	show White = "x"
	show Black = "o"
	show Empty = "_"

instance Eq Player where
	White == White = True
	Black == Black = True
	Empty == Empty = True
	_ == _ = False 

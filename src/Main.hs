module Main where

main :: IO ()
main = undefined
data Player = White | Black | Empty
type Field = [[Player]]
type Revert = Maybe [Field]

instance Show Player where
	show White = "x"
	show Black = "o"
	show Empty = "_"
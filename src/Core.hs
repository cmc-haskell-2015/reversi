module Core where
import Main

--printField::Field->IO()
--printField _ = print ("" ++ "")

initialize::Field
initialize = new_game (ini_lines 8)

new_game::Field->Field
new_game f = push_checker(push_checker (push_checker (push_checker 
	f 4 4 Black) 5 5 Black) 4 5 White) 5 4 White

--tostring::Field->String
--tostring [] = ""
--tostring (x:xs) = (linetostring x) ++  ++ (tostring xs)

--linetostring::[Player]->String
--linetostring [] = ""
--linetostring (x:xs) = (show x) ++ " " ++ linetostring xs
--
--printb::Field->IO()
--printb [] = ""
--printb (x:xs) = putStrLn (linetostring x) : printb xs

push_checker::Field->Int->Int->Player->Field
push_checker [] _ _ _ = []
push_checker (f:fs) x y p
	| x == 1 = (push_checker_line f y p) : push_checker fs 0 0 p
	| otherwise = f : push_checker fs (x - 1) y p

push_checker_line::[Player]->Int->Player->[Player]
push_checker_line [] _ _ = []
push_checker_line (f:fs) y p
	| y == 1 = p : push_checker_line fs 0 p
	| otherwise = f : push_checker_line fs (y - 1) p

ini_lines::Int->Field
ini_lines 0 = []
ini_lines x = ini_cells 8 : ini_lines (x-1)

ini_cells::Int->[Player]
ini_cells 0 = []
ini_cells x = Empty : ini_cells (x-1)

core :: IO ()
core = undefined
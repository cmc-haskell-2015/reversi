module Core where
import Main

initialize::Move
initialize = (new_game (ini_lines 8), Black, 2, 2)

new_game::Field->Field
new_game f = push_checkers f (
	(1, 1, Black):
	(8, 8, Black):
	(1, 8, White):
	(8, 1, White):
	(4, 4, Black):
	(4, 5, Black):
	(5, 4, White):
	(4, 3, White):
	(3, 4, Black):
	(5, 5, White):
	(5, 3, Black):
	[])

linetostring::[Player]->String
linetostring [] = ""
linetostring (x:xs) = (show x) ++ " " ++ linetostring xs

printm::Move->IO()
printm (f, p, x, y) = do
	printb f
	putStrLn ("Move: " ++ (show p))
	putStrLn ("Account: " ++ (show x) ++ ":" ++ (show y))

printb::Field->IO()
printb [] = putStrLn ""
printb (x:xs) = do
	putStrLn (linetostring x)
	printb xs


push_checkers::Field->[(Int, Int, Player)]->Field
push_checkers f [] = f
push_checkers f ((x, y, p):xs) = push_checkers (push_checker f x y p) xs

mpush_checker::Move->Int->Int->Player->Move
mpush_checker (f, a, b, c) x y p = ((push_checker f x y p), a, b, c)

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

look::Move->Int->Int->Maybe Player
look (f,_,_,_) x y 
	| (x > 0 && x <= 8 && y > 0 && y <= 8) = Just (f!!(x-1)!!(y-1))
	| otherwise = Nothing


check::Move->Int->Int->[Int]
check f x y 
	|(look f x y) == Just Empty = filter (walk f x y) (directions f x y)
	|otherwise = []

getpos::Int->Int->Int->(Int, Int)
getpos x y 1 = (x-1, y-1)
getpos x y 2 = (x-1, y)
getpos x y 3 = (x-1, y+1)
getpos x y 4 = (x, y-1)
getpos x y 5 = (x, y)
getpos x y 6 = (x, y+1)
getpos x y 7 = (x+1, y-1)
getpos x y 8 = (x+1, y)
getpos x y 9 = (x+1, y+1)
getpos _ _ _ = (0, 0)

getplayer::Move->Player
getplayer (_,p,_,_) = p

directions::Move->Int->Int->[Int]
directions f a b = range f a b 1

range::Move->Int->Int->Int->[Int]
range _ _ _ 10 = []
range f x y i 
	|(look f u v == Just White) && (i /= 5) = (i : range f x y (i+1))
	|otherwise = range f x y (i+1)
	where 
		u = fst (getpos x y i)
		v = snd (getpos x y i)
		p = getplayer f

walk::Move->Int->Int->Int->Bool
walk f x y dir
	|(look f u v == Just Empty) || (look f u v == Nothing) = False
	|(look f u v == Just p) = True
	|otherwise = walk f u v dir
	where 
		u = fst (getpos x y dir)
		v = snd (getpos x y dir)
		p = getplayer f

revert::Move->Int->Int->Int->Move
revert f x y dir
	|(look f u v == Just Empty) || (look f u v == Nothing) = f
	|(look f u v == Just p) = f
	|otherwise = revert (mpush_checker f u v p) u v dir
	where 
		u = fst (getpos x y dir)
		v = snd (getpos x y dir)
		p = getplayer f

core :: IO ()
core = undefined
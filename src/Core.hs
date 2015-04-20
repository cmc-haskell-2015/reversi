module Core where
import Main
import Data.Char

initialize::Move
-- Инициализация игрового поля
initialize = (newGame (iniLines 8), White, 2, 2)

newGame::Field->Field
-- Расстановка начальной позиции
newGame f = pushCheckers f (
    (4, 4, Black):
    (5, 5, Black):
    (4, 5, White):
    (5, 4, White):
    [])

lineToString::[Player]->String
-- Для печати поля
lineToString [] = ""
lineToString (x:xs) = (show x) ++ " " ++ lineToString xs

printm::Move->IO()
-- Печать поля на очередном ходе, чей ход и счёт
printm (f, p, x, y) = do
    printb f
    putStrLn ("Move: " ++ (show p))
    putStrLn ("Account: " ++ (show x) ++ ":" ++ (show y))

printb::Field->IO()
-- Печать игрового поля (_ - пусто, x - белые, o - чёрные)
printb [] = putStrLn ""
printb (x:xs) = do
    putStrLn (lineToString x)
    printb xs


pushCheckers::Field->[(Int, Int, Player)]->Field
-- Изменение ячейеек поля
pushCheckers f [] = f
pushCheckers f ((x, y, p):xs) = pushCheckers (pushChecker f x y p) xs

mpushChecker::Move->Int->Int->Player->Move
-- Изменение ячейки для структуры
mpushChecker (f, a, b, c) x y p = ((pushChecker f x y p), a, b, c)

pushChecker::Field->Int->Int->Player->Field
-- Изменение ячейки поля
pushChecker [] _ _ _ = []
pushChecker (f:fs) x y p
    | x == 1 = (pushCheckerLine f y p) : pushChecker fs 0 0 p
    | otherwise = f : pushChecker fs (x - 1) y p

pushCheckerLine::[Player]->Int->Player->[Player]
-- Изменение ячейки поля: техническая часть
pushCheckerLine [] _ _ = []
pushCheckerLine (f:fs) y p
    | y == 1 = p : pushCheckerLine fs 0 p
    | otherwise = f : pushCheckerLine fs (y - 1) p

iniLines::Int->Field
-- Инициализация горизонтали
iniLines 0 = []
iniLines x = iniCells 8 : iniLines (x-1)

iniCells::Int->[Player]
-- Инициализация ячеек в горизонтали
iniCells 0 = []
iniCells x = Empty : iniCells (x-1)

look::Move->Int->Int->Maybe Player
-- Получение значения ячейки по горизонтали и вертикали
look (f,_,_,_) x y 
    | (x > 0 && x <= 8 && y > 0 && y <= 8) = Just (f!!(x-1)!!(y-1))
    | otherwise = Nothing


check::Move->Int->Int->[Int]
-- Проверка допустимости хода
check f x y 
    |(look f x y) == Just Empty = filter (walk f x y) (directions f x y)
    |otherwise = []

getPos::Int->Int->Int->(Int, Int)
-- Конвертация направления в координаты
getPos x y 1 = (x-1, y-1)    -- влево вверх
getPos x y 2 = (x-1, y)      -- вверх
getPos x y 3 = (x-1, y+1)    -- вправо вверх
getPos x y 4 = (x, y-1)      -- влево
getPos x y 5 = (x, y)        -- никуда (центр)
getPos x y 6 = (x, y+1)      -- вправо
getPos x y 7 = (x+1, y-1)    -- влево вниз
getPos x y 8 = (x+1, y)      -- вниз
getPos x y 9 = (x+1, y+1)    -- вправо вниз
getPos _ _ _ = (0, 0)

getPlayer::Move->Player
-- Получение игрока из структуры
getPlayer (_,p,_,_) = p

directions::Move->Int->Int->[Int]
-- Получение возможных направлений для переворота
directions f a b = range f a b 1

opponent::Player->Player
-- Получение оппонента игрока
opponent White = Black
opponent Black = White
opponent _ = Empty

range::Move->Int->Int->Int->[Int]
-- Получение списка направлений для переворота
range _ _ _ 10 = []
range f x y i 
    |(look f u v == Just (opponent p)) && (i /= 5) = (i : range f x y (i+1))
    |otherwise = range f x y (i+1)
    where 
        u = fst (getPos x y i)
        v = snd (getPos x y i)
        p = getPlayer f

walk::Move->Int->Int->Int->Bool
-- Проверка направления на допустимость переворота
walk f x y dir
    |(look f u v == Just Empty) || (look f u v == Nothing) = False
    |(look f u v == Just p) = True
    |otherwise = walk f u v dir
    where 
        u = fst (getPos x y dir)
        v = snd (getPos x y dir)
        p = getPlayer f

revert::Move->Int->Int->Int->Move
-- Переворот шашек по направлению
revert f x y dir
    |(look f u v == Just Empty) || (look f u v == Nothing) = f
    |(look f u v == Just p) = f
    |otherwise = revert (mpushChecker f u v p) u v dir
    where 
        u = fst (getPos x y dir)
        v = snd (getPos x y dir)
        p = getPlayer f

revertAll::Move->Int->Int->[Int]->Move
-- Переворот шашек по всем допустимым направлениям
revertAll f _ _ [] = f
revertAll f x y (z:zs) = revert (revertAll f x y zs) x y z

countCheckers::Field->Player->Int
-- Пересчёт количества шашек после хода
countCheckers f p = length (filter (==p) (foldr (++) [] f))

recount::Move->Move
-- Инициализация пересчёта шашек и смена игрока, делающего свой ход
recount (f, p, _, _) 
    | p == White = (f, Black, (countCheckers f Black), (countCheckers f White))
    |p == Black = (f, White, (countCheckers f Black), (countCheckers f White))
    |otherwise = (f, Empty, 0, 0)

move::Move->Int->Int->Revert
-- Ход игрока, если это возможно
move f x y | dirs == [] = Nothing
        |otherwise = Just (recount (revertAll (mpushChecker f x y p) x y dirs))
    where 
        dirs = check f x y
        p = getPlayer f

start::Move->IO()
-- Запуск консольной версии игры
start board = do
    printm board
    if (not (canMove board)) then do
        putStrLn "No moves for player"
        nextMove 0 board (Just board)
    else do
        putStr "Move> "
        x <- getChar
        _ <- getChar
        y <- getChar
        _ <- getChar
        nextMove (digitToInt x) board (move board (digitToInt x) (digitToInt y))

resultGame::Move->Maybe Player
-- Результат игры
resultGame (_, _, x, y) 
    | x == 0 = Just White -- Если у игрока не осталось шашек,
    | y == 0 = Just Black -- победил его оппонент
    | (x + y == 64) && x > y = Just Black -- Если всё поле заполнено, то
    | (x + y == 64) && x < y = Just White -- побеждает тот, у кого больше шашек
    | x == 32 && y == 32 = Just Empty -- Ничья, если шашек поровну
    | otherwise = Nothing

checkWinner::Move->Bool
-- Проверка, есть ли победитель после очередного хода
checkWinner f 
    | resultGame f == Nothing = False
    | otherwise = True

winner::Maybe Player->IO()
-- Печать победителя (или ничьи)
winner (Just White) = putStrLn "White wins!"
winner (Just Black) = putStrLn "Black wins!"
winner _ = putStrLn "Draw"

checkField::Move->Int->Int->Bool
-- Проверка поля на возможность хода с булевым результатом
checkField f x y | check f x y == [] = False
    |otherwise = True

canMove::Move->Bool
-- Проверка возможности хода
canMove f = foldr (||) False [checkField f x y| x <- [1..8], y <- [1..8]]

switchPlayer::Move->Move
-- Переход хода в случае невозможности
switchPlayer (f, p, w, b) = (f, (opponent p), w, b)

nextMove::Int->Move->Revert->IO()
-- Инициализация следующего хода
nextMove 0 (f, p, b, w) _ = start (f, (opponent p), b, w) -- Пас
nextMove 9 _ _ = putStrLn "End." -- Принудительное прерывание
nextMove _ prev Nothing = do
    putStrLn "Wrong move"
    start prev
nextMove _ _ (Just next) | checkWinner next = do
        printm next
        winner (resultGame next)
    |otherwise = start next

core :: IO ()
core = undefined
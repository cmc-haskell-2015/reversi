module Core where
import Db
import Types
import Data.Char
import Database.Persist.TH
import Data.Text (Text, pack, unpack) 
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Time 
import Data.Time.Clock

--------------------------------------------------------------------------------
-- Основные функции
--------------------------------------------------------------------------------

-- Запуск игры
initGame :: State
initGame = State (newGame $ iniLines 8) Black (2, 2)

-- Расстановка начальной позиции
newGame :: Field -> Field
newGame f = pushCheckers f (
    ((4, 4), Black) :
    ((5, 5), Black) :
    ((4, 5), White) :
    ((5, 4), White) :
    [])

-- Инициализация горизонтали
iniLines::Int->Field
iniLines 0 = []
iniLines x = iniCells 8 : iniLines (x-1)

-- Инициализация ячеек в горизонтали
iniCells::Int->[Player]
iniCells 0 = []
iniCells x = Empty : iniCells (x-1)

-- Изменение ячейки поля
push :: State -> Cell -> Player -> State
push s c p = State f pl sc
    where
        f = pushChecker (board s) c p
        pl = player s
        sc = score s

-- Изменение ячееек поля
pushCheckers :: Field -> [(Cell, Player)] -> Field
pushCheckers f [] = f
pushCheckers f ((c, p):xs) = pushCheckers (pushChecker f c p) xs

-- Изменение ячейки поля
pushChecker :: Field -> Cell -> Player -> Field
pushChecker [] _ _ = []
pushChecker (f:fs) (x, y) p  
    | x == 1 = (pushCheckerLine f y p) : pushChecker fs (0, 0) p
    | otherwise = f : pushChecker fs (x - 1, y) p

-- Изменение ячейки поля: техническая часть
pushCheckerLine :: [Player] -> Int -> Player -> [Player]
pushCheckerLine [] _ _ = []
pushCheckerLine (f:fs) y p
    | y == 1 = p : pushCheckerLine fs 0 p
    | otherwise = f : pushCheckerLine fs (y - 1) p

-- Получение значения ячейки по горизонтали и вертикали
look :: State -> Cell -> Maybe Player
look s (x, y)
    | (x > 0 && x <= 8 && y > 0 && y <= 8) = Just (f!!(x-1)!!(y-1))
    | otherwise = Nothing
    where f = (board s)

-- Проверка допустимости хода
check :: State -> Cell -> [Int]
check s c
    |(look s c) == Just Empty = filter (walk s c) (directions s c)
    |otherwise = []

-- Конвертация направления в координаты
getPos :: Cell -> Int -> Cell
getPos (x, y) 1 = (x-1, y-1)    -- влево вверх
getPos (x, y) 2 = (x-1, y)      -- вверх
getPos (x, y) 3 = (x-1, y+1)    -- вправо вверх
getPos (x, y) 4 = (x, y-1)      -- влево
getPos (x, y) 5 = (x, y)        -- никуда (центр)
getPos (x, y) 6 = (x, y+1)      -- вправо
getPos (x, y) 7 = (x+1, y-1)    -- влево вниз
getPos (x, y) 8 = (x+1, y)      -- вниз
getPos (x, y) 9 = (x+1, y+1)    -- вправо вниз
getPos _ _ = (0, 0)

-- Получение возможных направлений для переворота
directions :: State -> Cell -> [Int]
directions s c = range s c 1

-- Получение оппонента игрока
opponent :: Player -> Player
opponent White = Black
opponent Black = White
opponent _ = Empty

-- Получение списка направлений для переворота
range :: State -> Cell -> Int -> [Int]
range _ _ 10 = []
range s c i 
    |(look s n == Just (opponent p)) && (i /= 5) = (i : range s c (i+1))
    |otherwise = range s c (i+1)
    where 
        p = (player s)
        n = (getPos c i)

-- Проверка направления на допустимость переворота
walk :: State -> Cell -> Int -> Bool
walk s c dir
    |(look s n == Just Empty) || (look s n == Nothing) = False
    |(look s n == Just p) = True
    |otherwise = walk s n dir
    where
        p = (player s)
        n = (getPos c dir)

-- Переворот шашек по направлению
revert :: State -> Cell -> Int -> State
revert s c dir
    |(look s n == Just Empty) || (look s n == Nothing) = s
    |(look s n == Just p) = s
    |otherwise = revert (push s n p) n dir
    where 
        p = (player s)
        n = (getPos c dir)

-- Переворот шашек по всем допустимым направлениям
revertAll :: State -> Cell -> [Int] -> State
revertAll s _ [] = s
revertAll s c (z:zs) = revert (revertAll s c zs) c z

-- Пересчёт количества шашек после хода
countCheckers :: Field -> Player -> Int
countCheckers f p = length (filter (==p) (foldr (++) [] f))

-- Инициализация пересчёта шашек и смена игрока, делающего свой ход
recount :: State -> State
recount s = State (board s) (opponent $ player s) (w, b)
    where
        w = (countCheckers (board s) White)
        b = (countCheckers (board s) Black)

-- Ход игрока, если это возможно
mov :: State -> Cell -> Maybe State
mov s c | dirs == [] = Nothing
        |otherwise = Just (recount (revertAll (push s c p) c dirs))
    where 
        dirs = check s c
        p = (player s)

-- Результат игры
resultGame :: State -> Maybe Player
resultGame s
    | x == 0 = Just White -- Если у игрока не осталось шашек,
    | y == 0 = Just Black -- победил его оппонент
    | (x + y == 64) && x > y = Just Black -- Если всё поле заполнено, то
    | (x + y == 64) && x < y = Just White -- побеждает тот, у кого больше шашек
    | x == 32 && y == 32 = Just Empty -- Ничья, если шашек поровну
    | otherwise = Nothing
    where
        x = fst $ score s
        y = snd $ score s

-- Проверка, есть ли победитель после очередного хода
checkWinner :: State -> Bool
checkWinner s 
    | resultGame s == Nothing = False
    | otherwise = True

-- Проверка поля на возможность хода с булевым результатом
checkField :: State -> Cell -> Bool
checkField s c | check s c == [] = False
    |otherwise = True

-- Проверка возможности хода
canMov :: State -> Bool
canMov s = foldr (||) False [checkField s (x, y)| x <- [1..8], y <- [1..8]]

-- Переход хода в случае невозможности
switchMove :: State -> State
switchMove s = State f (opponent p) sc
    where
        f = (board s)
        p = (player s)
        sc = (score s)



--------------------------------------------------------------------------------
-- Функции для работы с консольным интерфейсом игры
--------------------------------------------------------------------------------

-- Для печати поля
lineToString :: [Player] -> String
lineToString [] = ""
lineToString (x:xs) = (show x) ++ " " ++ lineToString xs

-- Печать игрового поля (_ - пусто, x - белые, o - чёрные)
printb :: Field -> IO ()
printb [] = putStrLn ""
printb (x:xs) = do
    putStrLn (lineToString x)
    printb xs
    return ()

-- Печать текущего состояния
prints :: State -> IO ()
prints s = do
    printb (board s)
    putStrLn ("Move: " ++ (show (player s)))
    putStrLn ("Account: " ++ (show $ fst $ score s) 
        ++ ":" ++ (show $ snd $ score s))

-- Запуск консольной версии игры
startCLI :: State -> IO ()
startCLI board = do
    prints board
    if (not (canMov board)) then do
        putStrLn "No moves for player"
        nextMove 0 board (Just board)
    else do
        putStr "Move> "
        x <- getChar
        _ <- getChar
        y <- getChar
        _ <- getChar
        nextMove (digitToInt x) board (mov board (digitToInt x, digitToInt y))

-- Печать победителя (или ничьи)
winnerCLI :: Maybe Player -> IO ()
winnerCLI (Just White) = putStrLn "White wins!"
winnerCLI (Just Black) = putStrLn "Black wins!"
winnerCLI _ = putStrLn "Draw"

-- Инициализация следующего хода
nextMove :: Int -> State -> Maybe State -> IO ()
nextMove 0 s _ = startCLI (switchMove s) -- Пас
nextMove 9 s _ = do
    t0 <- getCurrentTime
    saveGame s "auto" (show t0)
    putStrLn "End." -- Принудительное прерывание
nextMove _ prev Nothing = do
    putStrLn "Wrong move"
    startCLI prev
nextMove _ _ (Just next) | checkWinner next = do
        prints next
        winnerCLI (resultGame next)
    |otherwise = startCLI next

--------------------------------------------------------------------------------
-- Функции для cохранения и загрузки игры
--------------------------------------------------------------------------------

-- Упаковка доски в список Int
packBoard :: State -> [Int]
packBoard s = map packPlayer $ foldr (++) [] b
    where b = board s

-- Упаковка игрока в Int
packPlayer :: Player -> Int
packPlayer Empty = 0
packPlayer White = 1
packPlayer Black = 2

-- Распаковка игрока из Int
unpackPlayer :: Int -> Player
unpackPlayer 0 = Empty
unpackPlayer 1 = White
unpackPlayer 2 = Black

-- Распаков доски из списка Int
unpackBoard :: [Int] -> Field
unpackBoard l = [line l y | y <- [0..7]]

line :: [Int] -> Int -> [Player]
line x y = map unpackPlayer $ take 8 $ drop (y * 8) x

-- Сохранить игру: состояние поля, имя сейва, время сейва
saveGame :: State -> String -> String -> IO ()
saveGame s name time = runSqlite dbPath $ do
    runMigration migrateAll
    insert $ Save name (packBoard s) (packPlayer $ player s) time
    return ()

-- Загрузка игры из сейва в БД
loadGame :: Maybe String -> IO ()
loadGame Nothing = query unpackRow 
    "SELECT board, move FROM Save ORDER BY time DESC LIMIT 0, 1"
loadGame (Just name) = query unpackRow $
    "SELECT board, move FROM Save WHERE name='" ++ name
    ++ "' ORDER BY time DESC LIMIT 0, 1"

-- Распаковка строки сейва в БД
unpackRow :: [PersistValue] -> IO ()
unpackRow (s:(p:ps)) = do
    unpackState s p
    return ()

-- Распаковка списка из БД
fromString :: String -> [Int]
fromString [] = []
fromString (x:xs) 
    | x == '0' = 0:fromString xs
    | x == '1' = 1:fromString xs
    | x == '2' = 2:fromString xs
    | otherwise = fromString xs

-- Распаковка игрока из БД
unpackPl :: String -> Player
unpackPl [] = Empty
unpackPl (s:xs)
    | s == '1' = White
    | s == '2' = Black
    | otherwise = Empty

-- Распаковка состояния и возобновление игры из сейва
unpackState :: PersistValue -> PersistValue -> IO ()
unpackState s p = startCLI
   $ recount $ State 
   (unpackBoard $ fromString $ unpack $ right $ fromPersistValueText s)
   (opponent $ unpackPl $ unpack $ right $ fromPersistValueText p)
   (0, 0)
-- | ???
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
import Data.List.Split

-- * Основные функции

-- | Запуск игры.
initGame :: State
initGame = State (newGame $ iniLines 8) Black (2, 2)

-- | Расстановка начальной позиции.
newGame :: Field -> Field
newGame f = pushCheckers f (
    ((4, 4), Black) :
    ((5, 5), Black) :
    ((4, 5), White) :
    ((5, 4), White) :
    [])

-- | Инициализация горизонтали.
iniLines::Int->Field
iniLines 0 = []
iniLines x = iniCells 8 : iniLines (x-1)

-- | Инициализация ячеек в горизонтали.
iniCells::Int->[Player]
iniCells 0 = []
iniCells x = Empty : iniCells (x-1)

-- | Изменение ячейки поля.
push :: State -> Cell -> Player -> State
push s c p = State f pl sc
    where
        f = pushChecker (board s) c p
        pl = player s
        sc = score s

-- | Изменение ячееек поля.
pushCheckers :: Field -> [(Cell, Player)] -> Field
pushCheckers f [] = f
pushCheckers f ((c, p):xs) = pushCheckers (pushChecker f c p) xs

-- | Изменение ячейки поля.
pushChecker :: Field -> Cell -> Player -> Field
pushChecker [] _ _ = []
pushChecker (f:fs) (x, y) p  
    | x == 1 = (pushCheckerLine f y p) : pushChecker fs (0, 0) p
    | otherwise = f : pushChecker fs (x - 1, y) p

-- | Изменение ячейки поля: техническая часть.
pushCheckerLine :: [Player] -> Int -> Player -> [Player]
pushCheckerLine [] _ _ = []
pushCheckerLine (f:fs) y p
    | y == 1 = p : pushCheckerLine fs 0 p
    | otherwise = f : pushCheckerLine fs (y - 1) p

-- | Получение значения ячейки по горизонтали и вертикали.
look :: State -> Cell -> Maybe Player
look s (x, y)
    | (x > 0 && x <= 8 && y > 0 && y <= 8) = Just (f!!(x-1)!!(y-1))
    | otherwise = Nothing
    where f = (board s)

-- | Проверка допустимости хода.
check :: State -> Cell -> [Int]
check s c
    |(look s c) == Just Empty = filter (walk s c) (directions s c)
    |otherwise = []

-- | Конвертация направления в координаты.
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

-- | Получение возможных направлений для переворота.
directions :: State -> Cell -> [Int]
directions s c = range s c 1

-- | Получение оппонента игрока.
opponent :: Player -> Player
opponent White = Black
opponent Black = White
opponent _ = Empty

-- | Получение списка направлений для переворота.
range :: State -> Cell -> Int -> [Int]
range _ _ 10 = []
range s c i 
    |(look s n == Just (opponent p)) && (i /= 5) = (i : range s c (i+1))
    |otherwise = range s c (i+1)
    where 
        p = (player s)
        n = (getPos c i)

-- | Проверка направления на допустимость переворота.
walk :: State -> Cell -> Int -> Bool
walk s c dir
    |(look s n == Just Empty) || (look s n == Nothing) = False
    |(look s n == Just p) = True
    |otherwise = walk s n dir
    where
        p = (player s)
        n = (getPos c dir)

-- | Переворот шашек по направлению.
revert :: State -> Cell -> Int -> State
revert s c dir
    |(look s n == Just Empty) || (look s n == Nothing) = s
    |(look s n == Just p) = s
    |otherwise = revert (push s n p) n dir
    where 
        p = (player s)
        n = (getPos c dir)

-- | Переворот шашек по всем допустимым направлениям.
revertAll :: State -> Cell -> [Int] -> State
revertAll s _ [] = s
revertAll s c (z:zs) = revert (revertAll s c zs) c z

-- | Пересчёт количества шашек после хода.
countCheckers :: Field -> Player -> Int
countCheckers f p = length (filter (==p) (foldr (++) [] f))

-- | Инициализация пересчёта шашек и смена игрока, делающего свой ход.
recount :: State -> State
recount s = State (board s) (opponent $ player s) (w, b)
    where
        w = (countCheckers (board s) White)
        b = (countCheckers (board s) Black)

-- | Ход игрока, если это возможно.
mov :: State -> Cell -> Maybe State
mov s c | dirs == [] = Nothing
        |otherwise = Just (recount (revertAll (push s c p) c dirs))
    where 
        dirs = check s c
        p = (player s)

-- | Результат игры.
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

-- | Проверка, есть ли победитель после очередного хода.
checkWinner :: State -> Bool
checkWinner s 
    | resultGame s == Nothing = False
    | otherwise = True

-- | Проверка поля на возможность хода с булевым результатом.
checkField :: State -> Cell -> Bool
checkField s c | check s c == [] = False
    |otherwise = True

-- | Проверка возможности хода.
canMov :: State -> Bool
canMov s = foldr (||) False [checkField s (x, y)| x <- [1..8], y <- [1..8]]

-- | Переход хода в случае невозможности.
switchMove :: State -> State
switchMove s = State f (opponent p) sc
    where
        f = (board s)
        p = (player s)
        sc = (score s)

-- * Рекорды

-- | Запись результата игры в таблицу.
saveRecord :: State -> String -> IO ()
saveRecord s t = runSqlite dbPath $ do
    runMigration migrateAll
    insert $ Record (packMaybe winner) x y t
    return ()
    where 
        winner = resultGame s
        x = fst $ score s
        y = snd $ score s

-- | Вывод 10 лучших игр по счёту, при равном счёте -- по дате игры.
showRecords :: IO ()
showRecords = query unpackRecord $
    "SELECT winner, white, black, time FROM Record " ++ 
    "ORDER BY MAX(white, black) ASC, time DESC LIMIT 0, 10"

-- | Печать строчки рекорда на экран.
unpackRecord :: [PersistValue] -> IO ()
unpackRecord [] = putStrLn ""
unpackRecord (winner:white:black:time:[]) = do
    putStr $ show $ unpackPl $ unpack $ right $ fromPersistValueText winner
    putStr " "
    putStr $ unpack $ right $ fromPersistValueText white
    putStr ":"
    putStr $ unpack $ right $ fromPersistValueText black
    putStr " "
    putStr $ unpack $ right $ fromPersistValueText time
    putStrLn ""
    return ()

-- * Функции для cохранения и загрузки игры

-- | Упаковка доски в список @'Int'@.
packBoard :: State -> [Int]
packBoard s = map packPlayer $ foldr (++) [] b
    where b = board s

-- | Упаковка игрока в @'Int'@.
packPlayer :: Player -> Int
packPlayer Empty = 0
packPlayer White = 1
packPlayer Black = 2

-- | Упаковка игрока в @'Int'@.
packMaybe :: Maybe Player -> Int
packMaybe (Just White) = 1
packMaybe (Just Black) = 2
packMaybe _ = 0

-- | Распаковка игрока из @'Int'@.
unpackPlayer :: Int -> Player
unpackPlayer 0 = Empty
unpackPlayer 1 = White
unpackPlayer 2 = Black

-- | Распаковка доски из списка @'Int'@.
unpackBoard :: [Int] -> Field
unpackBoard l = [line l y | y <- [0..7]]

-- | ???
line :: [Int] -> Int -> [Player]
line x y = map unpackPlayer $ take 8 $ drop (y * 8) x

-- | Сохранить игру: состояние поля, имя сейва, время сейва.
saveGame :: State -> String -> String -> IO ()
saveGame s name time = runSqlite dbPath $ do
    runMigration migrateAll
    insert $ Save name (packBoard s) (packPlayer $ player s) time
    return ()

-- | Распаковка списка из БД.
fromString :: String -> [Int]
fromString [] = []
fromString (x:xs) 
    | x == '0' = 0:fromString xs
    | x == '1' = 1:fromString xs
    | x == '2' = 2:fromString xs
    | otherwise = fromString xs

-- | Распаковка игрока из БД.
unpackPl :: String -> Player
unpackPl [] = Empty
unpackPl (s:xs)
    | s == '1' = White
    | s == '2' = Black
    | otherwise = Empty


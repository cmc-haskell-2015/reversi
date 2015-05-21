-- | Функции для работы с консольным интерфейсом игры.
module Interface.CLI where

import Core
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

-- | Для печати поля.
lineToString :: [Player] -> String
lineToString [] = ""
lineToString (x:xs) = (show x) ++ " " ++ lineToString xs

-- | Печать игрового поля (@_@ - пусто, @x@ - белые, @o@ - чёрные).
printb :: Field -> IO ()
printb [] = putStrLn ""
printb (x:xs) = do
    putStrLn (lineToString x)
    printb xs
    return ()

-- | Печать текущего состояния.
prints :: State -> IO ()
prints s = do
    printb (board s)
    putStrLn ("Move: " ++ (show (player s)))
    putStrLn ("Account: " ++ (show $ fst $ score s) 
        ++ ":" ++ (show $ snd $ score s))


-- | Запуск консольной версии игры.
startCLI :: State -> IO ()
startCLI board = do
    prints board
    if (not (canMov board)) then do
        putStrLn "No moves for player"
        nextMove 0 board (Just board)
    else do
        putStr "Move> "
        s <- getLine
        parseCLI board $ splitOn " " s
        return ()

-- | Парсинг команды от игрока.
parseCLI :: State -> [String] -> IO ()
parseCLI board [] = do
    putStr "Move> "
    s <- getLine
    parseCLI board $ splitOn " " s
parseCLI board (x:(y:ys)) 
    | x == "save" = do
        t0 <- getCurrentTime
        saveGame board y $ show t0
        parseCLI board []
    | x == "load" = do
        loadGame $ Just y
    | otherwise = nextMove (read x :: Int) board 
        (mov board (read x :: Int, read y :: Int))
parseCLI board (x:xs)       
    | x == "exit" = do
        putStrLn "End."
        return () 

-- | Печать победителя (или ничьи).
winnerCLI :: Maybe Player -> IO ()
winnerCLI (Just White) = putStrLn "White wins!"
winnerCLI (Just Black) = putStrLn "Black wins!"
winnerCLI _ = putStrLn "Draw"

-- | Инициализация следующего хода.
nextMove :: Int -> State -> Maybe State -> IO ()
nextMove 0 s _ = startCLI (switchMove s) -- Пас
nextMove _ prev Nothing = do
    putStrLn "Wrong move"
    startCLI prev
nextMove _ _ (Just next) | checkWinner next = do
        t0 <- getCurrentTime
        prints next
        winnerCLI (resultGame next)
        saveRecord next (show t0)
    |otherwise = startCLI next

-- * Загрузка игры из сохранения

-- | Распаковка строки сейва в БД.
unpackRow :: [PersistValue] -> IO ()
unpackRow (s:(p:ps)) = do
    unpackState s p
    return ()

-- | Распаковка состояния и возобновление игры из сейва.
unpackState :: PersistValue -> PersistValue -> IO ()
unpackState s p = startCLI
   $ recount $ State 
   (unpackBoard $ fromString $ unpack $ right $ fromPersistValueText s)
   (opponent $ unpackPl $ unpack $ right $ fromPersistValueText p)
   (0, 0)

-- | Загрузка игры из сейва в БД.
loadGame :: Maybe String -> IO ()
loadGame Nothing = query unpackRow 
    "SELECT board, move FROM Save ORDER BY time DESC LIMIT 0, 1"
loadGame (Just name) = query unpackRow $
    "SELECT board, move FROM Save WHERE name='" ++ name
    ++ "' ORDER BY time DESC LIMIT 0, 1"


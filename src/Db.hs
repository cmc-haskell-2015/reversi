{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | ???
module Db where

import Database.Persist.TH
import Data.Text (Text, pack, unpack) 
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Types

-- | Путь до файла с базой данных.
dbPath = "../db/db.bin"

-- Таблицы в БД
--
-- * @Save@ - таблица с сейвами: имя сейва, позиция на доске, чей ход, дата сейва
-- * @Record@ - таблица завершённых игр: чья победа, счёт, дата игры
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Save
    name String
    board [Int]
    move Int
    time String

Record
    winner Int
    white Int
    black Int
    time String
|]

-- | Запрос к базе данных. Первый аргумент - функция, которая обрабатывает
-- полученные результаты.
query :: ([PersistValue] -> IO ()) -> String -> IO ()
query f s = runSqlite dbPath $ do 
    runMigration migrateAll
    rawQuery sql [] $$ CL.mapM_ (liftIO . f)
    where sql = pack s

-- | Пример работы с запросом: печать всех игр, начиная с последней.
dumpTable :: IO ()
dumpTable = query print "SELECT * FROM Record ORDER BY time DESC"

-- | Выбор правого элемента для распаковки @'Either'@.
right :: Either a b -> b
right (Right b) = b

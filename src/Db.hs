{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Db where

import Database.Persist.TH
import Data.Text (Text, pack, unpack) 
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Types

dbPath = "../db/db.bin"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Save
    name String
    board [Int]
    move Int
|]



query :: ([PersistValue] -> IO ()) -> String -> IO ()
query f s = runSqlite dbPath $ do 
    runMigration migrateAll
    rawQuery sql [] $$ CL.mapM_ (liftIO . f)
    where sql = pack s

dumpTable :: IO ()
dumpTable = query print "SELECT * FROM Save"

--main :: IO ()
--main = runSqlite "../db/db.bin" $ do
--    runMigration migrateAll
--    insert $ Person "Michael Snoyman"
--    insert $ Person "Miriam Snoyman"
--    insert $ Person "Eliezer Snoyman"
--    insert $ Person "Gavriella Snoyman"
--    insert $ Person "Greg Weber"
--    insert $ Person "Rick Richardson"

--    -- Persistent does not provide the LIKE keyword, but we'd like to get the
--    -- whole Snoyman family...
--    let sql = "SELECT name FROM Person WHERE name LIKE '%Snoyman'"
--    rawQuery sql [] $$ CL.mapM_ (liftIO . out)



--out :: [PersistValue] -> IO ()
--out (x:xs) = out1 x

--out1 :: PersistValue -> IO ()
--out1 x = print $ right $ fromPersistValueText x

right :: Either a b -> b
right (Right b) = b

--dumpTable :: IO ()
--dumpTable = query "SELECT * FROM Person"
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
    time String
|]

query :: ([PersistValue] -> IO ()) -> String -> IO ()
query f s = runSqlite dbPath $ do 
    runMigration migrateAll
    rawQuery sql [] $$ CL.mapM_ (liftIO . f)
    where sql = pack s

dumpTable :: IO ()
dumpTable = query print "SELECT * FROM Save ORDER BY time DESC"

right :: Either a b -> b
right (Right b) = b

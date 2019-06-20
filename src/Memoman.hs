{-# LANGUAGE OverloadedStrings #-}

module Memoman (dbName, addCards) where

import qualified SimpleCard as S
import qualified Database as D
import Prelude hiding (FilePath)
import System.Environment
import Data.Time.Clock (getCurrentTime, UTCTime)
import Shelly hiding (run)
import Data.Text (Text, pack)

dbName = fromText ".memoman"

addCards :: FilePath -> Sh ()
addCards path = do
    dbm <- D.readDatabase dbName
    db <- case dbm of
            Nothing -> error "No database found. Try running init."
            Just x -> return x
    newDb <- S.addCards db path
    D.writeDatabase newDb dbName

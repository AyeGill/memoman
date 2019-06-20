{-# LANGUAGE OverloadedStrings #-}

module Memoman (dbName, addCards, review) where

import qualified CliViewer as C
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

maybeReviewEntry :: (D.Entry, Float) -> Sh (D.Entry, Float) --Entry and q-value
maybeReviewEntry (e, prevQ) = if prevQ < 4
    then do
        today <- liftIO getCurrentTime
        textm <- S.findText e
        case textm of
            Nothing -> do
                echo "Error: didn't find card for entry:"
                liftIO $ print e
                echo "Skipping..."
                return (e, 5) --return 5 so we don't keep trying this card.
            Just t -> do
                let (q,a) = S.splitCard t
                q <- C.review q a
                return $ (D.review e q today, q)
    else return (e,prevQ)

reviewEntry e = maybeReviewEntry (e,-1)

runSession :: [D.Entry] -> Sh [D.Entry] --Run a session with repetition
runSession entries = map fst <$> (go $ map (\e -> (e,-1)) entries)
                where go :: [(D.Entry, Float)] -> Sh [(D.Entry, Float)]
                      go esqs = if all (\(e,q) -> q>=4) esqs
                            then return esqs
                            else go =<< (sequence $ map maybeReviewEntry esqs)

review :: FilePath -> Sh ()
review path = do
    --putStrLn "reviewing.."
    dbm <- D.readDatabase path
    db <- case dbm of
            Nothing -> error "No databse found. Try running init."
            Just x -> return x
    --putStrLn "Found database. Continuing..."
    today <- liftIO getCurrentTime
    let entries = D.toReview today db
    --putStrLn "Found entries to review.."
    newEntries <- runSession entries
    --putStrLn "Session run"
    let newDb = D.modifyEntries db newEntries
    D.writeDatabase newDb path
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified CliViewer as C
import qualified SimpleCard as S
import qualified Database as D
import System.Environment
import Data.Time.Clock (getCurrentTime, UTCTime)

dbName = ".memoman"

(</>) a b = a ++ "/" ++ b 
infixr 2 </>

main = do
    args <- getArgs
    run args

run args = case args of
    ["init"] -> run ["init", "."]
    ["review"] -> run ["review", "."]
    ["add-cards", path] -> addCards path --assumes database is in cwd.
    ["init", path] -> D.writeDatabase (D.mkDatabase []) (path </> dbName)
    ["review", path] -> review (path </> dbName)
    ["dump-db"] -> D.readDatabase dbName >>= print --testing
    _ -> putStrLn "Invalid arguments"

addCards :: FilePath -> IO ()
addCards path = do
    dbm <- D.readDatabase dbName
    db <- case dbm of
            Nothing -> error "No database found. Try running init."
            Just x -> return x
    newDb <- S.addCards db path
    D.writeDatabase newDb dbName

maybeReviewEntry :: (D.Entry, Float) -> IO (D.Entry, Float) --Entry and q-value
maybeReviewEntry (e, prevQ) = if prevQ < 4
    then do
        today <- getCurrentTime
        textm <- S.findText e
        case textm of
            Nothing -> do
                putStrLn "Error: didn't find card for entry:"
                print e
                putStrLn "Skipping..."
                return (e, 5) --return 5 so we don't keep trying this card.
            Just t -> do
                let (q,a) = S.splitCard t
                q <- C.review q a
                return $ (D.review e q today, q)
    else return (e,prevQ)

reviewEntry e = maybeReviewEntry (e,-1)

runSession :: [D.Entry] -> IO [D.Entry] --Run a session with repetition
runSession entries = map fst <$> (go $ map (\e -> (e,-1)) entries)
                where go :: [(D.Entry, Float)] -> IO [(D.Entry, Float)]
                      go esqs = if all (\(e,q) -> q>=4) esqs
                            then return esqs
                            else go =<< (sequence $ map maybeReviewEntry esqs)



review :: FilePath -> IO ()
review path = do
    --putStrLn "reviewing.."
    dbm <- D.readDatabase path
    db <- case dbm of
            Nothing -> error "No databse found. Try running init."
            Just x -> return x
    --putStrLn "Found database. Continuing..."
    today <- getCurrentTime
    let entries = D.toReview today db
    --putStrLn "Found entries to review.."
    newEntries <- runSession entries
    --putStrLn "Session run"
    let newDb = D.modifyEntries db newEntries
    D.writeDatabase newDb path
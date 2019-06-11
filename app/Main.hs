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

reviewEntry :: D.Entry -> IO D.Entry
reviewEntry e = do
    today <- getCurrentTime
    textm <- S.findText e
    case textm of
        Nothing -> do
            putStrLn "Error: didn't find card for entry:"
            print e
            putStrLn "Skipping..."
            return e
        Just t -> do
            let (q,a) = S.splitCard t
            q <- C.review q a
            return $ D.review e q today


review :: FilePath -> IO ()
review path = do
    dbm <- D.readDatabase path
    db <- case dbm of
            Nothing -> error "No databse found. Try running init."
            Just x -> return x
    today <- getCurrentTime
    let entries = D.toReview today db
    newEntries <- sequence $ map reviewEntry entries
    let newDb = D.modifyEntries db newEntries
    D.writeDatabase newDb path


main = do
    args <- getArgs
    run args
{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main
    ( main
    )
where

import           Prelude                 hiding ( FilePath )
import qualified Database                      as D
import           System.Environment
import           Shelly                  hiding ( run )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified CliViewer                     as C
import qualified SimpleCard                    as S
import           Memoman
import           Data.Time.Clock                ( getCurrentTime )
import Data.IORef

main = do
    args <- getArgs
    shelly $ run (map pack args)

run :: [Text] -> Sh ()
run args = case args of
    ["init"      ]      -> run ["init", "."]
    ["review"    ]      -> run ["review", "."]
    ["add-cards", path] -> addCards (fromText path) --assumes database is in cwd.
    ["init", path] ->
        D.writeDatabase (D.mkDatabase []) (fromText path </> dbName)
    ["review", path]     -> review (fromText path </> dbName)
    ["dump-db"]          -> D.readDatabase dbName >>= (liftIO . print) --testing
    _                    -> echo "Invalid arguments"

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
                let (q, a) = S.splitCard t
                q <- C.review q a
                return $ (D.review e q today, q)
    else return (e, prevQ)

reviewEntry e = maybeReviewEntry (e, -1)

runSession :: [D.Entry] -> Sh [D.Entry] --Run a session with repetition
runSession entries = map fst <$> (go $ map (, -1) entries)
  where
    go :: [(D.Entry, Float)] -> Sh [(D.Entry, Float)]
    go esqs = if all (\(e, q) -> q >= 4) esqs
        then return esqs
        else go =<< (mapM maybeReviewEntry esqs)

review :: FilePath -> Sh ()
review path = do
    (db, entries) <- getEntries path
    --putStrLn "Found entries to review.."
    newEntries <- runSession entries
    --putStrLn "Session run"
    let newDb = D.modifyEntries db newEntries
    D.writeDatabase newDb path

getEntries :: FilePath -> Sh (D.Database, [D.Entry])
getEntries path = do
    dbm <- D.readDatabase path
    db <- case dbm of
        Nothing -> error "No database found. Try running memoman init."
        Just x -> return x
    today <- liftIO getCurrentTime
    return $ (db, D.toReview today db)
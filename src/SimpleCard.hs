{-# LANGUAGE OverloadedStrings #-}

module SimpleCard (
    findText,
    splitCard,
    addCards
) where

import qualified Database as D
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.IO (readFile, writeFile)
import Prelude hiding (readFile, writeFile)
import Data.List
import Data.UUID.V4
import Data.UUID hiding (fromText)
import SuperMemo as S
import Data.Time.Clock
import Shelly hiding (FilePath, find, trace)
import Debug.Trace (trace)
-- Cards stored as raw text in simple files, no preprocessing.
-- cards separated by "---", Q/A separated by newlines.
-- Each card starts with ID:[UUID]


findText :: D.Entry -> IO (Maybe T.Text)
findText e = do
    textM <- findRawText e
    case textM of
        Nothing -> return Nothing
        Just x -> Just <$> runFile x

-- Given a database entry, pick out the associated card.
-- Should be rewritten to be more robust, readable, ...
findRawText :: D.Entry -> IO (Maybe T.Text)
findRawText entry = do
    contents <- readFile $ D.getPath entry
    return $ go $ contents
    where go bytes = fmap (T.unlines . tail)
                   $ find (checkId $ D.getId entry)
                   $ map T.lines 
                   $ map T.strip
                   $ T.splitOn "\n---" bytes
          checkId id lines = (head lines) == T.concat ["ID:", (T.pack $ show id)]


runCommand :: T.Text -> T.Text -> IO T.Text -- add error handling?
runCommand cmd input = shelly 
    $ handleany_sh (\_ -> return $ T.concat ["ERROR OCURRED WHILE RUNNING COMMAND ", cmd]) 
    $ silently $ do
        (return input) -|- (run (fromText cmd) [])

runFile :: T.Text -> IO T.Text --If beginning with a shebang, run it.
runFile t = case T.lines t of
    [] -> return ""
    hd:tl -> if (T.head hd)=='#'
        then runCommand (T.tail hd) $ T.unlines tl
        else return $ T.unlines $ hd:tl

splitCard :: T.Text -> (T.Text, T.Text)
splitCard x = (T.strip q, T.strip a)
    where (q,a) = T.breakOn "\n\n" x


--Find ID-less cards in the file, add them to the database with fresh SM data
addCards :: D.Database -> FilePath -> IO D.Database
addCards base path = do
    lines <- T.lines <$> (readFile path)
    (newLines, ids) <- helper lines
    writeFile path $ T.unlines newLines
    entries <- sequence $ map (\id -> D.mkEntry id path <$> (S.mkLearningData <$> getCurrentTime)) ids
    return $ D.insertEntries base entries
    where helper :: [T.Text] -> IO ([T.Text], [UUID])
          helper [] = return ([],[])
          helper (x:xs) = do
            (rest, ids) <- helper xs
            if x /= "ID:" --hacked, but new cards start with "ID:".
            then return (x:rest, ids)
            else do
                id <- nextRandom
                return ((T.append "ID:" $ T.pack $ show id):rest,id:ids)
          -- iterate over lines, adding ids where necessary and collecting ids in a list
{-# LANGUAGE OverloadedStrings #-}

module SimpleCard where

import qualified Database as D
import qualified Data.Text as T
import Data.Text.IO (readFile, writeFile)
import Prelude hiding (readFile, writeFile)
import Data.List
import Data.UUID.V4
import Data.UUID
import SuperMemo as S
import Data.Time.Clock
-- Cards stored as raw text in simple files, no preprocessing.
-- cards separated by "---", Q/A separated by newlines.
-- Each card starts with ID:[UUID]

-- Given a database entry, pick out the associated card.
-- Should be rewritten to be more robust, readable, ...
findText :: D.Entry -> IO (Maybe T.Text)
findText entry = go <$> (readFile $ D.getPath entry)
    where go bytes = fmap (T.intercalate "\n" . tail)
                   $ find (checkId $ D.getId entry) 
                   $ map (T.splitOn "\n") 
                   $ T.splitOn "---" bytes
          checkId id lines = (head lines) == T.intercalate "" ["ID:", (T.pack $ show id)]

splitCard :: T.Text -> (T.Text, T.Text)
splitCard x = (T.strip q, T.strip a)
    where (q,a) = T.breakOn "\n\n" x


--Find ID-less cards in the file, add them to the database with fresh SM data
addCards :: D.Database -> FilePath -> IO D.Database
addCards base path = do
    lines <- T.lines <$> (readFile path)
    (newLines, ids) <- helper lines
    writeFile path $ T.unlines newLines
    entries <- sequence $ map (\id -> D.E id path <$> (S.mkLearningData <$> getCurrentTime)) ids
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
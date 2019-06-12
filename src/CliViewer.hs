{-# LANGUAGE OverloadedStrings #-}

module CliViewer (review) where

import Data.Text as T
import Data.Text.IO (putStrLn, getLine)
import Prelude hiding (putStrLn, getLine)

review :: T.Text -> T.Text -> IO Float
review question answer = do
    putStrLn "Q:"
    putStrLn question
    putStrLn "Press enter to view answer."
    _ <- getLine
    putStrLn answer
    putStrLn "Rate your recall 0-5. 0 total blackout, 3 recalled but difficult, 5 is easy."
    qin <- T.strip <$> getLine
    return $ read $ T.unpack qin

{-# LANGUAGE OverloadedStrings #-}

module CliViewer (review) where

import Data.Text as T
import Data.Text.IO (putStrLn, getLine)
import Prelude hiding (putStrLn, getLine)
import Shelly

review :: T.Text -> T.Text -> Sh Float
review question answer = do
    echo "Q:"
    echo question
    echo "Press enter to view answer."
    _ <- liftIO getLine
    echo answer
    echo "Rate your recall 0-5. 0 total blackout, 3 recalled but difficult, 5 is easy."
    qin <- T.strip <$> liftIO getLine
    return $ read $ T.unpack qin

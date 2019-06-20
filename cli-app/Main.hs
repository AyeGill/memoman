{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified CliViewer as C
import qualified SimpleCard as S
import qualified Database as D
import Prelude hiding (FilePath)
import System.Environment
import Data.Time.Clock (getCurrentTime, UTCTime)
import Shelly hiding (run)
import Data.Text (Text, pack)
import Memoman

main = do
    args <- getArgs
    shelly $ run (map pack args)

run :: [Text] -> Sh ()
run args = case args of
    ["init"] -> run ["init", "."]
    ["review"] -> run ["review", "."]
    ["add-cards", path] -> addCards (fromText path) --assumes database is in cwd.
    ["init", path] -> D.writeDatabase (D.mkDatabase []) (fromText path </> dbName)
    ["review", path] -> review (fromText path </> dbName)
    ["dump-db"] -> D.readDatabase dbName >>= (liftIO . print) --testing
    _ -> echo "Invalid arguments"
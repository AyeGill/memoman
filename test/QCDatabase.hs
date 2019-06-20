{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

module QCDatabase (main) where

import Prelude hiding (FilePath)
import Database
import SuperMemo
import QCSuperMemo hiding (main)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances.Text
import Test.QuickCheck.Instances.UUID
import Test.QuickCheck.Instances.Time
import Data.Time.Clock
import Shelly

instance Arbitrary FilePath where
    arbitrary = Shelly.fromText <$> arbitrary

instance Arbitrary Entry where
    arbitrary = mkEntry <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Database where
    arbitrary = mkDatabase <$> listOf arbitrary

reviewsMustReview :: Database -> UTCTime -> Bool
reviewsMustReview db today = all (\e -> (nextReview $ getLD e)<today) entries
    where entries = toReview today db


myPath = fromText "TESTDATABASE"
readWrite :: Database -> Sh Bool
readWrite db = do
    writeDatabase db myPath
    mDb <- readDatabase myPath
    rm myPath
    case mDb of
        Nothing -> return False
        Just newDb -> return (db==newDb)

readWriteTest :: Database -> Property
readWriteTest db = monadicIO $ shelly $ readWrite db

insertVsMkDb :: [Entry] -> Bool
insertVsMkDb es = (inserted == mkDatabase es) && (inserted == inserted')
    where inserted = insertEntries (mkDatabase []) es
          inserted' = foldl insertEntry (mkDatabase []) es

updateTiming :: Entry -> Float -> UTCTime -> Property
updateTiming e q t = (t >= (nextReview (getLD e))) ==> (nextReview (getLD new) >= nextReview (getLD e)) 
    where new = review e q t

main = do
    putStrLn "Testing: ReviewMustReview"
    quickCheck reviewsMustReview
    putStrLn "Testing: readWriteTest"
    quickCheck readWriteTest
    putStrLn "Testing: Insert vs mkDB"
    quickCheck insertVsMkDb
    putStrLn "Testing: update timing"
    quickCheck updateTiming

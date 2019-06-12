{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

module Database (
    ID
    , Database
    , mkDatabase
    , Entry
    , mkEntry -- Is this bad encapsulation
    , readDatabase
    , writeDatabase
    , toReview
    , getPath
    , getId
    , modifyEntries
    , insertEntries
    , review --rename this?
) where

import qualified Data.ByteString as B
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import qualified SuperMemo as S
import qualified Data.List.Ordered as L
import qualified Data.Time.Clock as T (UTCTime)
import Data.Serialize
import GHC.Generics
import SerialUUID

rightToMaybe = either (const Nothing) Just
-- Pick out the right of an Either, return nothing if left.

--Every card has an UID, which is associated in the database to:
-- * A filepath - note that a given file may contain multiple cards.
--   to recover a card, a filepath and an UID is recquired.
-- * A set of learning data, for the SuperMemo algorithm.

-- Currently, requires reading and writing entire database each time.
-- (Not cards themselves, just metadata)
-- Even for databases with a million cards, this should be megabytes of memory, so probably fine.
-- But think about memory overhead?

type ID = UUID

data Entry = E !ID !FilePath !S.LearningData deriving (Eq, Generic, Show)
mkEntry = E
instance Serialize Entry
-- Note that we violate the Ord laws, since compare disregards everything but the times,
-- unlike (==). This should be fine, given the timing precision,
-- But still write code considering this.
instance Ord Entry where
    compare (E _ _ ld) (E _ _ ld') = compare (S.nextReview ld) (S.nextReview ld') 

getId :: Entry -> ID
getId (E id _ _) = id

getPath :: Entry -> FilePath
getPath (E _ path _) = path


-- *sorted* list of entries, by next review - early to late.
-- We shouldn't export the constructor.
data Database = D [Entry] deriving (Eq, Generic, Show)
instance Serialize Database

-- Constructor which sorts input
mkDatabase :: [Entry] -> Database
mkDatabase = D . L.sort

-- Take those entries with dates earlier than today.
toReview :: T.UTCTime -> Database -> [Entry]
toReview time (D base) = takeWhile mustReview base
    where mustReview (E _ _ ld) = S.nextReview ld < time
    -- if next review is earlier than today.

-- Delete entry(s) with the same id, and insert this one at the correct place.
modifyEntry :: Database -> Entry -> Database 

modifyEntry (D base) entry@(E id _ _)=
    D $ L.insertBag entry $ filter isNotOld $ base
    where isNotOld (E id' _ _) = id /= id'

modifyEntries :: Database -> [Entry] -> Database
modifyEntries base entries = 
    foldl modifyEntry base entries

insertEntry :: Database -> Entry -> Database
insertEntry (D base) entry = D $ L.insertBag entry base

insertEntries :: Database -> [Entry] -> Database
insertEntries = foldl insertEntry
-- Utility fn. Does S.update on the learningdata part.
review :: Entry -> Float -> T.UTCTime-> Entry
review (E id path ld) q today = E id path $ S.update ld q today

readDatabase :: FilePath -> IO (Maybe Database)
readDatabase path = do
    bytes <- B.readFile path
    return $ rightToMaybe $ decode bytes

writeDatabase :: Database -> FilePath -> IO ()
writeDatabase base path = B.writeFile path $ encode base


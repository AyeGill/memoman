{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

module Database (
    ID
    , Database
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
-- Even for databases with millions of cards, this should be <100KB, so fine.

type ID = UUID

data Entry = E ID FilePath S.LearningData deriving (Eq, Generic)
instance Serialize Entry
-- Note that we violate the Ord laws, since compare disregards everything but the times,
-- unlike (==). This should be fine, given the timing precision,
-- But still write code considering this.
instance Ord Entry where
    compare (E _ _ ld) (E _ _ ld') = compare (S.nextReview ld) (S.nextReview ld') 


-- *sorted* list of entries, by next review - early to late.
-- We shouldn't export the constructor.
data Database = D [Entry] deriving Generic
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


readDatabase :: FilePath -> IO (Maybe Database)
readDatabase path = do
    bytes <- B.readFile path
    return $ rightToMaybe $ decode bytes
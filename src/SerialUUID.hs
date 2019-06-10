module SerialUUID (
    
) where

import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize
import Data.UUID
import Data.Word (Word32)
import Data.Time.Clock



uuidPut :: Putter Word32 -> Putter UUID
uuidPut word32 x = case toWords x of
    (a, b, c, d) -> word32 a <> word32 b <> word32 c <> word32 d

uuidGet :: Get Word32 -> Get UUID
uuidGet word32 = fromWords <$> word32 <*> word32 <*> word32 <*> word32


instance Serialize UUID where
    put = uuidPut putWord32le
    get = uuidGet getWord32le
  
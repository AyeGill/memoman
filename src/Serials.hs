module Serials (
    
) where

import Prelude hiding (FilePath)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize
import Data.UUID (UUID, fromWords, toWords)
import Data.Word (Word32)
import Shelly (FilePath, fromText, toTextIgnore)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

instance Serialize Text where
    put = put . TE.encodeUtf8
    get = TE.decodeUtf8 <$> get 


uuidPut :: Putter Word32 -> Putter UUID
uuidPut word32 x = case toWords x of
    (a, b, c, d) -> word32 a <> word32 b <> word32 c <> word32 d

uuidGet :: Get Word32 -> Get UUID
uuidGet word32 = fromWords <$> word32 <*> word32 <*> word32 <*> word32

instance Serialize UUID where
    put = uuidPut putWord32le
    get = uuidGet getWord32le


fpPut :: Putter Text -> Putter FilePath
fpPut text x = text $ toTextIgnore $ x

fpGet :: Get Text -> Get FilePath
fpGet text = fromText <$> text

instance Serialize FilePath where
    put = fpPut put
    get = fpGet get
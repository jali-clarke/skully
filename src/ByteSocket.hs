module ByteSocket (
    ByteSocket(..)
) where

import Data.Word (Word8)

class ByteSocket m where
    getByte :: m Word8
    putByte :: Word8 -> m ()
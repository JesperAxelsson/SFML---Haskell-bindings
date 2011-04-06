module SFML.ForeignUtils where

import Foreign
import Foreign.C
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI

withT :: Storable a => a -> (Ptr a -> IO b) -> IO b
withT = with

peekFree :: Storable a => Ptr a -> IO a
peekFree ptr = do
  x <- peek ptr
  free ptr
  return x

cToEnum :: Enum a => CInt -> a
cToEnum e = toEnum (fromIntegral e)

cFromEnum :: Enum a => a -> CInt
cFromEnum e = fromIntegral (fromEnum e)

withByteString :: ByteString -> (Ptr CUChar -> IO a) -> IO a
withByteString bytes m =
  let (bytesFPtr, offset, _len) = BSI.toForeignPtr bytes
  in withForeignPtr bytesFPtr $ \bytesPtr ->
      m (plusPtr (castPtr bytesPtr) offset)
    

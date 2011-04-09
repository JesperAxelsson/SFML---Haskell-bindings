module SFML.ForeignUtils where

import Foreign
import Foreign.C
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI

withT :: Storable a => a -> (Ptr a -> IO b) -> IO b
withT = with

castWithT :: Storable a => a -> (Ptr b -> IO c) -> IO c
castWithT x a = with x (a.castPtr)

cToEnum :: Enum a => CInt -> a
cToEnum e = toEnum (fromIntegral e)

cFromEnum :: Enum a => a -> CInt
cFromEnum e = fromIntegral (fromEnum e)

withByteString :: ByteString -> (Ptr b -> IO a) -> IO a
withByteString bytes m =
  let (bytesFPtr, offset, _len) = BSI.toForeignPtr bytes
  in withForeignPtr bytesFPtr $ \bytesPtr ->
      m (plusPtr (castPtr bytesPtr) offset)
    
withByteStringLen :: ByteString -> ((Ptr (), CULong) -> IO a) -> IO a
withByteStringLen bytes m =
  let (bytesFPtr, offset, len) = BSI.toForeignPtr bytes
  in withForeignPtr bytesFPtr $ \bytesPtr ->
      m (castPtr (plusPtr bytesPtr offset), fromIntegral len)

withMaybe :: (a -> (Ptr a -> IO b) -> IO b) -> Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe withFoo Nothing act = act nullPtr
withMaybe withFoo (Just x) act = withFoo x act

fromNull :: (Ptr a -> IO a) -> Ptr a -> IO (Maybe a)
fromNull conv ptr | ptr == nullPtr = return Nothing
                  | otherwise = fmap Just $ conv ptr

module SFML.ForeignUtils where

import Foreign
import Foreign.C
import Control.Monad
import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Word

withT :: Storable a => a -> (Ptr a -> IO b) -> IO b
withT = with

castWithT :: Storable a => a -> (Ptr b -> IO c) -> IO c
castWithT x a = with x (a.castPtr)

cToEnum :: Enum a => CInt -> a
cToEnum e = toEnum (fromIntegral e)

cFromEnum :: Enum a => a -> CInt
cFromEnum e = fromIntegral (fromEnum e)

withByteString :: a -> ByteString -> (Ptr a -> IO b) -> IO b
withByteString _ bytes m =
  let (bytesFPtr, offset, _len) = BSI.toForeignPtr bytes
  in withForeignPtr bytesFPtr $ \bytesPtr ->
      m (plusPtr (castPtr bytesPtr) offset)

withByteStringLen :: a -> ByteString -> ((Ptr a, CULong) -> IO b) -> IO b
withByteStringLen _ bytes m =
  let (bytesFPtr, offset, len) = BSI.toForeignPtr bytes
  in withForeignPtr bytesFPtr $ \bytesPtr ->
      m (castPtr (plusPtr bytesPtr offset), fromIntegral len)

withMaybe :: (a -> (Ptr a -> IO b) -> IO b) -> Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe withFoo Nothing act = act nullPtr
withMaybe withFoo (Just x) act = withFoo x act

fromNull :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
fromNull conv ptr | ptr == nullPtr = return Nothing
                  | otherwise = fmap Just $ conv ptr


withUnicodeString :: String -> (Ptr CUInt -> IO a) -> IO a
withUnicodeString str act= do
  allocaArray len $ \arr -> do
  pokeElemOff arr (len - 1) 0
  forM_ (zip str [0..]) $ \(char, i) -> do
    pokeElemOff arr i (fromIntegral . ord $ char)
  act arr
 where len = length str + 1

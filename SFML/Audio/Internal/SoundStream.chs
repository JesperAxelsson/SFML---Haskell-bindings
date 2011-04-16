{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Audio.Internal.SoundStream where

#include <SFML/Audio/SoundStream.h>

{#import SFML.Audio.Internal.Types #}
import SFML.ForeignUtils
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Word
import System.Mem.Weak

{#context lib="csfml-audio" prefix="sf" #}

instance Storable SoundStreamChunk where
  sizeOf _ = {#sizeof SoundStreamChunk #}
  alignment _ = {#alignof SoundStreamChunk #}
  peek ptr = do
    samplesPtr <- {#get SoundStreamChunk.Samples #} ptr
    samplesCount <- fmap fromIntegral $ {#get SoundStreamChunk.NbSamples #} ptr
    bytes <- BS.packCStringLen (castPtr samplesPtr, samplesCount * (sizeOf (undefined :: {#type Int16 #})))
    return (SoundStreamChunk bytes)
  poke ptr (SoundStreamChunk bytes) = do
    BSU.unsafeUseAsCStringLen bytes $ \(bytesPtr, size) -> do
      {#set SoundStreamChunk.Samples #} ptr (castPtr bytesPtr)
      {#set SoundStreamChunk.NbSamples #} ptr (fromIntegral (fromIntegral size `div` (sizeOf (undefined :: {#type Int16 #}))))

foreign import ccall unsafe "wrapper"
  wrapGetDataCallback :: (Ptr SoundStreamChunk -> Ptr () -> IO CInt) -> IO SoundStreamGetDataCallback

foreign import ccall unsafe "wrapper"
  wrapSeekCallback :: (CFloat -> Ptr () -> IO ()) -> IO SoundStreamSeekCallback

mkGetDataCallback :: GetDataCallback -> IO SoundStreamGetDataCallback
mkGetDataCallback callback = wrapGetDataCallback getDataCallback
  where
    getDataCallback chunkPtr userData_ = do
      bytes <- callback
      case bytes of
        Nothing -> return (fromBool False)
        Just bytes' -> poke chunkPtr (SoundStreamChunk bytes') >> return (fromBool True)

mkSeekCallback :: SeekCallback -> IO SoundStreamSeekCallback
mkSeekCallback callback = wrapSeekCallback seekCallback
  where
    seekCallback pos userData_ = callback (realToFrac pos)

foreign import ccall unsafe "&sfSoundStream_Destroy"
  soundStreamDestroy :: FinalizerPtr SoundStreamPtr
                        
mkSoundStreamPtr :: Ptr SoundStreamPtr -> IO SoundStreamPtr
mkSoundStreamPtr ptr = fmap SoundStreamPtr $ newForeignPtr soundStreamDestroy ptr

{#fun unsafe SoundStream_Create as soundStreamCreate_
 {id `SoundStreamGetDataCallback' 
 ,id `SoundStreamSeekCallback'
 ,fromIntegral `Word'
 ,fromIntegral `Word'
 ,id `Ptr ()'} -> `SoundStreamPtr' mkSoundStreamPtr* #}

soundStreamCreate :: GetDataCallback -> SeekCallback -> Word -> Word -> IO SoundStream
soundStreamCreate getDataCallback seekCallback channelsCount sampleRate = do
  getDataCallback' <- mkGetDataCallback getDataCallback
  seekCallback' <- mkSeekCallback seekCallback
  soundStreamPtr <- soundStreamCreate_ getDataCallback' seekCallback' channelsCount sampleRate nullPtr
  addFinalizer getDataCallback' (freeHaskellFunPtr getDataCallback')
  addFinalizer seekCallback' (freeHaskellFunPtr seekCallback')
  return (SoundStream soundStreamPtr getDataCallback' seekCallback')


{#fun unsafe SoundStream_Play as soundStreamPlay_
 {withSoundStream* `SoundStream'} -> `()' #}

soundStreamPlay :: SoundStream -> IO ()
soundStreamPlay stream = do
  soundStreamPlay_
  waitFor (fmap (/=Playing) (soundStreamGetStatus stream))

{#fun unsafe SoundStream_Pause as ^
 {withSoundStream* `SoundStream'} -> `()' #}

{#fun unsafe SoundStream_Stop as ^
 {withSoundStream* `SoundStream'} -> `()' #}

{#fun unsafe SoundStream_GetStatus as ^
 {withSoundStream* `SoundStream'} -> `SoundStatus' cToEnum #}

{#fun unsafe SoundStream_GetChannelsCount as ^
 {withSoundStream* `SoundStream'} -> `Word' fromIntegral #}

{#fun unsafe SoundStream_GetSampleRate as ^
 {withSoundStream* `SoundStream'} -> `Word' fromIntegral #}

{#fun unsafe SoundStream_SetPitch as ^
 {withSoundStream* `SoundStream'
 ,`Float'} -> `()' #}

{#fun unsafe SoundStream_SetVolume as ^
 {withSoundStream* `SoundStream'
 ,`Float'} -> `()' #}

{#fun unsafe SoundStream_SetPosition as ^
 {withSoundStream* `SoundStream'
 ,`Float'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe SoundStream_SetRelativeToListener as ^
 {withSoundStream* `SoundStream'
 ,`Bool'} -> `()' #}

{#fun unsafe SoundStream_SetMinDistance as ^
 {withSoundStream* `SoundStream'
 ,`Float'} -> `()' #}

{#fun unsafe SoundStream_SetAttenuation as ^
 {withSoundStream* `SoundStream'
 ,`Float'} -> `()' #}

{#fun unsafe SoundStream_SetPlayingOffset as ^
 {withSoundStream* `SoundStream'
 ,`Float'} -> `()' #}

{#fun unsafe SoundStream_SetLoop as ^
 {withSoundStream* `SoundStream'
 ,`Bool'} -> `()' #}

{#fun unsafe SoundStream_GetPitch as ^
 {withSoundStream* `SoundStream'} -> `Float' #}

{#fun unsafe SoundStream_GetVolume as ^
 {withSoundStream* `SoundStream'} -> `Float' #}

{#fun unsafe SoundStream_GetPosition as soundStreamGetPosition_
 {withSoundStream* `SoundStream'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

soundStreamGetPosition :: SoundStream -> IO (Float, Float, Float)
soundStreamGetPosition stream = do
  (x, y, z) <- soundStreamGetPosition_ stream
  return (realToFrac x, realToFrac y, realToFrac z)
  
{#fun unsafe SoundStream_IsRelativeToListener as ^
 {withSoundStream* `SoundStream'} -> `Bool' #}

{#fun unsafe SoundStream_GetMinDistance as ^
 {withSoundStream* `SoundStream'} -> `Float' #}

{#fun unsafe SoundStream_GetAttenuation as ^
 {withSoundStream* `SoundStream'} -> `Float' #}

{#fun unsafe SoundStream_GetLoop as ^
 {withSoundStream* `SoundStream'} -> `Bool' #}

{#fun unsafe SoundStream_GetPlayingOffset as ^
 {withSoundStream* `SoundStream'} -> `Float' #}

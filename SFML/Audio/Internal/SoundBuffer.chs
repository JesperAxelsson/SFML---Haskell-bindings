{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Audio.Internal.SoundBuffer where

#include <SFML/Audio/SoundBuffer.h>

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
import Data.Word

{#context lib="csfml-audio" prefix="sf" #}

foreign import ccall unsafe "&sfSoundBuffer_Destroy"
  soundBufferDestroy :: FinalizerPtr SoundBuffer

mkSoundBuffer :: Ptr SoundBuffer -> IO SoundBuffer
mkSoundBuffer ptr = fmap SoundBuffer $ newForeignPtr soundBufferDestroy ptr

mkConstSoundBuffer :: Ptr SoundBuffer -> IO SoundBuffer
mkConstSoundBuffer ptr = fmap SoundBuffer $ newForeignPtr_ ptr

{#fun unsafe SoundBuffer_CreateFromFile as ^
 {`String'} -> `SoundBuffer' mkSoundBuffer* #}

{#fun unsafe SoundBuffer_CreateFromMemory as ^
 {'withByteStringLen ()'* `ByteString'&} -> `SoundBuffer' mkSoundBuffer* #}

{#fun unsafe SoundBuffer_CreateFromSamples as ^
 {'withByteStringLenSizeOf (undefined :: CShort)'* `ByteString'&
 ,fromIntegral `Word'
 ,fromIntegral `Word'} -> `SoundBuffer' mkSoundBuffer* #}

{#fun unsafe SoundBuffer_Copy as ^
 {withSoundBuffer* `SoundBuffer'} -> `SoundBuffer' mkSoundBuffer* #}

{#fun unsafe SoundBuffer_SaveToFile as ^
 {withSoundBuffer* `SoundBuffer'
 ,`String'} -> `Bool' #}

{#fun unsafe SoundBuffer_GetSamples as soundBufferGetSamples_
 {withSoundBuffer* `SoundBuffer'} -> `Ptr CShort' id #}

soundBufferGetSamples :: SoundBuffer -> IO ByteString
soundBufferGetSamples buffer = do
  len <- soundBufferGetSamplesCount buffer
  arrPtr <- soundBufferGetSamples_ buffer
  BS.packCStringLen (castPtr arrPtr, fromIntegral len `div` sizeOf (undefined :: CShort))

{#fun unsafe SoundBuffer_GetSamplesCount as ^
 {withSoundBuffer* `SoundBuffer'} -> `Word' fromIntegral #}

{#fun unsafe SoundBuffer_GetSampleRate as ^
 {withSoundBuffer* `SoundBuffer'} -> `Word' fromIntegral #}

{#fun unsafe SoundBuffer_GetChannelsCount as ^
 {withSoundBuffer* `SoundBuffer'} -> `Word' fromIntegral #}

{#fun unsafe SoundBuffer_GetDuration as ^
 {withSoundBuffer* `SoundBuffer'} -> `Float' #}

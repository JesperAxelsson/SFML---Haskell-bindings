{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Audio.Internal.SoundBufferRecorder where

#include <SFML/Audio/SoundBufferRecorder.h>

{#import SFML.Audio.Internal.Types #}
{#import SFML.Audio.Internal.SoundBuffer #}
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Data.Word


{#context lib="csfml-audio" prefix="sf" #}

foreign import ccall unsafe "&sfSoundBufferRecorder_Destroy"
  soundBufferRecorderDestroy :: FinalizerPtr SoundBufferRecorder

mkSoundBufferRecorder :: Ptr SoundBufferRecorder -> IO SoundBufferRecorder
mkSoundBufferRecorder ptr = fmap SoundBufferRecorder $ newForeignPtr soundBufferRecorderDestroy ptr

{#fun unsafe SoundBufferRecorder_Create as ^
 {} -> `SoundBufferRecorder' mkSoundBufferRecorder* #}

{#fun unsafe SoundBufferRecorder_Start as ^
 {withSoundBufferRecorder* `SoundBufferRecorder'
 ,fromIntegral `Word'} -> `()' #}

{#fun unsafe SoundBufferRecorder_Stop as ^
 {withSoundBufferRecorder* `SoundBufferRecorder'} -> `()' #}

{#fun unsafe SoundBufferRecorder_GetBuffer as soundBufferRecorderGetBuffer_
 {withSoundBufferRecorder* `SoundBufferRecorder'} -> `SoundBuffer' mkConstSoundBuffer* #}

soundBufferRecorderGetBuffer :: SoundBufferRecorder -> IO SoundBuffer
soundBufferRecorderGetBuffer recorder = withSoundBufferRecorder recorder $ \_ -> do
  buffer <- soundBufferRecorderGetBuffer_ recorder
  soundBufferCopy buffer

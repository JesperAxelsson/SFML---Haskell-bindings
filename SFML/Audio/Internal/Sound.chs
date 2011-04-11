{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Audio.Internal.Sound where


#include <SFML/Audio/Sound.h>

{#import SFML.Audio.Internal.Types #}
import SFML.ForeignUtils
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Data.IORef

{#context lib="csfml-audio" prefix="sf" #}

foreign import ccall unsafe "&sfSound_Destroy"
  soundDestroy :: FinalizerPtr SoundPtr

mkSound :: Ptr SoundPtr -> IO Sound
mkSound ptr = do
  soundPtr <- fmap SoundPtr $ newForeignPtr soundDestroy ptr
  soundBuffer <- newIORef Nothing
  return (Sound soundPtr soundBuffer)

{#fun unsafe Sound_Create as ^
 {} -> `Sound' mkSound* #}

{#fun unsafe Sound_Copy as ^
 {withSound* `Sound'} -> `Sound' mkSound* #}

{#fun unsafe Sound_Play as ^
 {withSound* `Sound'} -> `()' #}

{#fun unsafe Sound_Pause as ^
 {withSound* `Sound'} -> `()' #}

{#fun unsafe Sound_Stop as ^
 {withSound* `Sound'} -> `()' #}

{#fun unsafe Sound_SetBuffer as soundSetBuffer_
 {withSoundPtr* `SoundPtr'
 ,withSoundBuffer* `SoundBuffer'} -> `()' #}

soundSetBuffer :: Sound -> SoundBuffer -> IO ()
soundSetBuffer (Sound soundPtr soundBuffer) buffer = do
  writeIORef soundBuffer (Just buffer)
  soundSetBuffer_ soundPtr buffer

soundGetBuffer :: Sound -> IO (Maybe SoundBuffer)
soundGetBuffer = readIORef . soundBuffer

{#fun unsafe Sound_SetLoop as ^
 {withSound* `Sound'
 ,`Bool'} -> `()' #}

{#fun unsafe Sound_GetLoop as ^
 {withSound* `Sound'} -> `Bool' #}

{#fun unsafe Sound_GetStatus as ^
 {withSound* `Sound'} -> `SoundStatus' cToEnum #}

{#fun unsafe Sound_SetPitch as ^
 {withSound* `Sound'
 ,`Float'} -> `()' #}

{#fun unsafe Sound_SetVolume as ^
 {withSound* `Sound'
 ,`Float'} -> `()' #}

{#fun unsafe Sound_SetPosition as ^
 {withSound* `Sound'
 ,`Float'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Sound_SetRelativeToListener as ^
 {withSound* `Sound'
 ,`Bool'} -> `()' #}

{#fun unsafe Sound_SetMinDistance as ^
 {withSound* `Sound'
 ,`Float'} -> `()' #}

{#fun unsafe Sound_SetAttenuation as ^
 {withSound* `Sound'
 ,`Float'} -> `()' #}

{#fun unsafe Sound_SetPlayingOffset as ^
 {withSound* `Sound'
 ,`Float'} -> `()' #}

{#fun unsafe Sound_GetPitch as ^
 {withSound* `Sound'} -> `Float' #}

{#fun unsafe Sound_GetVolume as ^
 {withSound* `Sound'} -> `Float' #}

{#fun unsafe Sound_GetPosition as soundGetPosition_
 {withSound* `Sound'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

soundGetPosition :: Sound -> IO (Float, Float, Float)
soundGetPosition sound = do
  (x, y, z) <- soundGetPosition_ sound
  return (realToFrac x, realToFrac y, realToFrac z)

{#fun unsafe Sound_IsRelativeToListener as ^
 {withSound* `Sound'} -> `Bool' #}

{#fun unsafe Sound_GetMinDistance as ^
 {withSound* `Sound'} -> `Float' #}

{#fun unsafe Sound_GetAttenuation as ^
 {withSound* `Sound'} -> `Float' #}

{#fun unsafe Sound_GetPlayingOffset as ^
 {withSound* `Sound'} -> `Float' #}

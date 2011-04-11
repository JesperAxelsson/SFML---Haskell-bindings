{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Audio.Internal.Music where


#include <SFML/Audio/Music.h>

{#import SFML.Audio.Internal.Types #}
import SFML.ForeignUtils
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Data.ByteString (ByteString)
import Data.Word


{#context lib="csfml-audio" prefix="sf" #}

foreign import ccall unsafe "&sfMusic_Destroy"
  musicDestroy :: FinalizerPtr Music

mkMusic :: Ptr Music -> IO Music
mkMusic ptr = fmap Music $ newForeignPtr musicDestroy ptr

{#fun unsafe Music_CreateFromFile as ^
 {`String'} -> `Music' mkMusic* #}

{#fun unsafe Music_CreateFromMemory as ^
 {'withByteStringLen ()'* `ByteString'&} -> `Music' mkMusic* #}

{#fun unsafe Music_SetLoop as ^
 {withMusic* `Music'
 ,`Bool'} -> `()' #}

{#fun unsafe Music_GetLoop as ^
 {withMusic* `Music'} -> `Bool' #}

{#fun unsafe Music_GetDuration as ^
 {withMusic* `Music'} -> `Float' #}

{#fun unsafe Music_Play as ^
 {withMusic* `Music'} -> `()' #}

{#fun unsafe Music_Pause as ^
 {withMusic* `Music'} -> `()' #}

{#fun unsafe Music_Stop as ^
 {withMusic* `Music'} -> `()' #}

{#fun unsafe Music_GetChannelsCount as ^
 {withMusic* `Music'} -> `Word' fromIntegral #}

{#fun unsafe Music_GetSampleRate as ^
 {withMusic* `Music'} -> `Word' fromIntegral #}

{#fun unsafe Music_GetStatus as ^
 {withMusic* `Music'} -> `SoundStatus' cToEnum #}

{#fun unsafe Music_GetPlayingOffset as ^
 {withMusic* `Music'} -> `Float' #}

{#fun unsafe Music_SetPitch as ^
 {withMusic* `Music'
 ,`Float'} -> `()' #}

{#fun unsafe Music_SetVolume as ^
 {withMusic* `Music'
 ,`Float'} -> `()' #}

{#fun unsafe Music_SetPosition as ^
 {withMusic* `Music'
 ,`Float'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Music_SetRelativeToListener as ^
 {withMusic* `Music'
 ,`Bool'} -> `()' #}

{#fun unsafe Music_SetMinDistance as ^
 {withMusic* `Music'
 ,`Float'} -> `()' #}

{#fun unsafe Music_SetAttenuation as ^
 {withMusic* `Music'
 ,`Float'} -> `()' #}

{#fun unsafe Music_SetPlayingOffset as ^
 {withMusic* `Music'
 ,`Float'} -> `()' #}

{#fun unsafe Music_GetPitch as ^
 {withMusic* `Music'} -> `Float' #}

{#fun unsafe Music_GetVolume as ^
 {withMusic* `Music'} -> `Float' #}

{#fun unsafe Music_GetPosition as musicGetPosition_
 {withMusic* `Music'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

musicGetPosition :: Music -> IO (Float, Float, Float)
musicGetPosition music = do
  (x, y, z) <- musicGetPosition_ music
  return (realToFrac x, realToFrac y, realToFrac z)
  
{#fun unsafe Music_IsRelativeToListener as ^
 {withMusic* `Music'} -> `Bool' #}

{#fun unsafe Music_GetMinDistance as ^
 {withMusic* `Music'} -> `Float' #}

{#fun unsafe Music_GetAttenuation as ^
 {withMusic* `Music'} -> `Float' #}


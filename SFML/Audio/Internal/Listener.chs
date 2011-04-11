{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Audio.Internal.Listener where


#include <SFML/Audio/Listener.h>

{#import SFML.Audio.Internal.Types #}
import SFML.ForeignUtils
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils

{#context lib="csfml-audio" prefix="sf" #}

{#fun unsafe Listener_SetGlobalVolume as ^
 {`Float'} -> `()' #}

{#fun unsafe Listener_GetGlobalVolume as ^
 {} -> `Float' #}

{#fun unsafe Listener_SetPosition as ^
 {`Float', `Float', `Float'} -> `()' #}

{#fun unsafe Listener_GetPosition as listenerGetPosition_
 {alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

listenerGetPosition :: IO (Float, Float, Float)
listenerGetPosition = do
  (x, y, z) <- listenerGetPosition_
  return (realToFrac x, realToFrac y, realToFrac z)
  
{#fun unsafe Listener_SetDirection as ^
 {`Float', `Float', `Float'} -> `()' #}

{#fun unsafe Listener_GetDirection as listenerGetDirection_
 {alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

listenerGetDirection :: IO (Float, Float, Float)
listenerGetDirection = do
  (x, y, z) <- listenerGetDirection_
  return (realToFrac x, realToFrac y, realToFrac z)

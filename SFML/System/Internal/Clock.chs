{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.System.Internal.Clock where

#include <SFML/System/Clock.h>

{#import SFML.System.Internal.Types #}
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C

{#context lib="csfml-system" prefix="sf" #}

foreign import ccall unsafe "&sfClock_Destroy"
  clockDestroy :: FinalizerPtr Clock

mkClock :: Ptr Clock -> IO Clock
mkClock c = do
  c' <- newForeignPtr clockDestroy c
  return (Clock c')

{#fun unsafe Clock_Create as ^
 {} -> `Clock' mkClock* #}

{#fun unsafe Clock_GetTime as ^
 {withClock* `Clock'} -> `Float' #}

{#fun unsafe Clock_Copy as ^
 {withClock* `Clock'} -> `Clock' mkClock* #}

{#fun unsafe Clock_Reset as ^
 {withClock* `Clock'} -> `()' #}

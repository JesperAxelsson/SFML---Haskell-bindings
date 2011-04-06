{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SFML.System.Internal where

#include <SFML/System.h>

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Data.Word

{#context lib="csfml-system" prefix="sf" #}

{#pointer *Clock foreign newtype #}

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

{#fun unsafe Random_SetSeed as ^
 {fromIntegral `Word'} -> `()' #}

{#fun unsafe Random_GetSeed as ^
 {} -> `Word' fromIntegral #}

{#fun unsafe Random_Float as ^
 {`Float', `Float'} -> `Float' #}

{#fun unsafe Random_Int as ^
 {`Int', `Int'} -> `Int' #}


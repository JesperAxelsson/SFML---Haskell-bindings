{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.System.Internal.Randomizer where

#include <SFML/System/Randomizer.h>

import Foreign.C
import Data.Word

{#context lib="csfml-system" prefix="sf" #}

{#fun unsafe Random_SetSeed as ^
 {fromIntegral `Word'} -> `()' #}

{#fun unsafe Random_GetSeed as ^
 {} -> `Word' fromIntegral #}

{#fun unsafe Random_Float as ^
 {`Float', `Float'} -> `Float' #}

{#fun unsafe Random_Int as ^
 {`Int', `Int'} -> `Int' #}


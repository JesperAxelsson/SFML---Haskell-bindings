{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.System.Internal.Types where

#include <SFML/System/Types.h>

import Foreign.Ptr
import Foreign.ForeignPtr

{#context lib="csfml-system" prefix="sf" #}

{#pointer *Clock foreign newtype #}

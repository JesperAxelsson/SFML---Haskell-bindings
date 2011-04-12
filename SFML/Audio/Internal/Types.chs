{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Audio.Internal.Types where

#include <SFML/Audio/Types.h>
#include <SFML/Audio/SoundStatus.h>

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C
import Data.IORef

{#context lib="csfml-audio" prefix="sf" #}

{#pointer *Music foreign newtype #}

{#pointer *Sound as SoundPtr foreign newtype #}
data Sound = Sound { soundPtr :: SoundPtr
                   , soundBuffer :: IORef (Maybe SoundBuffer)
                   }
withSound = withSoundPtr . soundPtr

{#pointer *SoundBuffer foreign newtype #}

{#pointer *SoundBufferRecorder foreign newtype #}

{#pointer *SoundStream foreign newtype #}

{#enum SoundStatus {} deriving (Eq, Show) #}

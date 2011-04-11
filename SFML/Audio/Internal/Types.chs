{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Audio.Internal.Types where

#include <SFML/Audio/Types.h>
#include <SFML/Audio/SoundStatus.h>

import Foreign.ForeignPtr


{#context lib="csfml-audio" prefix="sf" #}

{#pointer *Music foreign newtype #}

{#pointer *Sound foreign newtype #}

{#pointer *SoundBuffer foreign newtype #}

{#pointer *SoundBufferRecorder foreign newtype #}

{#pointer *SoundRecorder foreign newtype #}

{#pointer *SoundStream foreign newtype #}

{#enum SoundStatus {} deriving (Eq, Show) #}

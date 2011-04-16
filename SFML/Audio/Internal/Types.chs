{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Audio.Internal.Types where

#include <SFML/Audio/Types.h>
#include <SFML/Audio/SoundStatus.h>
#include <SFML/Audio/SoundStream.h>

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C
import Data.ByteString (ByteString)
import Data.IORef
import Data.Word

{#context lib="csfml-audio" prefix="sf" #}

{#pointer *Music foreign newtype #}

{#pointer *Sound as SoundPtr foreign newtype #}
data Sound = Sound { soundPtr :: SoundPtr
                   , soundBuffer :: IORef (Maybe SoundBuffer)
                   }
withSound = withSoundPtr . soundPtr

{#pointer *SoundBuffer foreign newtype #}

{#pointer *SoundBufferRecorder foreign newtype #}

{#pointer *SoundStream as SoundStreamPtr foreign newtype #}
data SoundStream = SoundStream { soundStreamPtr :: SoundStreamPtr
                               , getDataCallback :: SoundStreamGetDataCallback
                               , seekCallback :: SoundStreamSeekCallback
                               }
withSoundStream = withSoundStreamPtr . soundStreamPtr
newtype SoundStreamChunk = SoundStreamChunk { chunkSamples :: ByteString
                                            }
type SoundStreamGetDataCallback = {#type SoundStreamGetDataCallback #}
type SoundStreamSeekCallback = {#type SoundStreamSeekCallback #}
type GetDataCallback = IO (Maybe ByteString)
type SeekCallback = Float -> IO ()

{#enum SoundStatus {} deriving (Eq, Show) #}

{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Window.Internal.VideoMode where

#include <SFML/Window/VideoMode.h>
#include "VideoModeWrapper.c"

{#import SFML.Window.Internal.Types #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.Storable

{#context lib="csfml-window" prefix="sf" #}

instance Storable VideoMode where
  sizeOf _ = {#sizeof VideoMode #}
  alignment _ = {#alignof VideoMode #}
  peek ptr = do
    width <- fmap fromIntegral $ {#get VideoMode->Width#} ptr
    height <- fmap fromIntegral $ {#get VideoMode->Height#} ptr
    bpp <- fmap fromIntegral $ {#get VideoMode->BitsPerPixel#} ptr
    return (VideoMode width height bpp)
  poke ptr (VideoMode width height bpp) = do
    {#set VideoMode->Width#} ptr (fromIntegral width)
    {#set VideoMode->Height#} ptr (fromIntegral height)
    {#set VideoMode->BitsPerPixel#} ptr (fromIntegral bpp)

{#fun VideoMode_GetDesktopModeWrapper as videoModeGetDesktopMode
 {alloca- `VideoMode' peek*} -> `()' #}

{#fun VideoMode_IsValidWrapper as videoModeIsValid
 {withT* `VideoMode'} -> `Bool' #}

{#fun VideoMode_GetFullscreenModes as videoModeGetFullscreenModes_
 {alloca- `CULong' peek*} -> `Ptr VideoMode' id #}

videoModeGetFullscreenModes :: IO [VideoMode]
videoModeGetFullscreenModes = do
  (ptrs, size) <- videoModeGetFullscreenModes_
  peekArray (fromIntegral size) ptrs

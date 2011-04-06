{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SFML.System.Internal where

#include <SFML/Window.h>
#include "VideoModeWrapper.c"

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Word
import Control.Monad

{#context lib="csfml-window" prefix="sf" #}

{-----------------------------------  Context  ------------------------------------}

{#pointer *Context foreign newtype #}

foreign import ccall unsafe "&sfContext_Destroy"
  contextDestroy :: FinalizerPtr Context

mkContext :: Ptr Context -> IO Context
mkContext ctx = do
  ctx' <- newForeignPtr contextDestroy ctx
  return (Context ctx')

{#fun unsafe Context_Create as ^
 {} -> `Context' mkContext* #}

{#fun unsafe Context_SetActive as ^
 {withContext* `Context', `Bool'} -> `()' #}

{------------------------------------  Events  ------------------------------------}

{#enum KeyCode {} deriving (Eq, Show) #}

{#enum MouseButton {} deriving (Eq, Show) #}

{#enum JoyAxis {} deriving (Eq, Show) #}

{#enum EventType {} deriving (Eq, Show) #}

cToEnum :: Enum a => CInt -> a
cToEnum e = toEnum (fromIntegral e)

cFromEnum :: Enum a => a -> CInt
cFromEnum e = fromIntegral (fromEnum e)

{------------------------------------  Input  ------------------------------------}

{#pointer *Input newtype #}

{#fun unsafe Input_IsKeyDown as ^
 {id `Input', cFromEnum `KeyCode'} -> `Bool' #}

{#fun unsafe Input_IsMouseButtonDown as ^
 {id `Input', cFromEnum `MouseButton'} -> `Bool' #}

{#fun unsafe Input_IsJoystickButtonDown as ^
 {id `Input', fromIntegral `Word', fromIntegral `Word'} -> `Bool' #}

{#fun unsafe Input_GetMouseX as ^
 {id `Input'} -> `Int' #}

{#fun unsafe Input_GetMouseY as ^
 {id `Input'} -> `Int' #}

{#fun unsafe Input_GetJoystickAxis as ^
 {id `Input', fromIntegral `Word', cFromEnum `JoyAxis'} -> `Float' #}

{----------------------------------  VideoMode  ----------------------------------}

data VideoMode = VideoMode {videoModeWidth, videoModeHeight, videoModeBitsPerPixel :: Word }
               deriving (Eq, Show)

{#pointer *VideoMode as VideoModePtr -> VideoMode #}

instance Storable VideoMode where
  sizeOf _ = {#sizeof VideoMode #}
  alignment _ = {#alignof VideoMode #}
  peek ptr = do
    width <- fmap fromIntegral $ {#get VideoMode->Width#} ptr
    height <- fmap fromIntegral $ {#get VideoMode->Height#} ptr
    bpp <- fmap fromIntegral $ {#get VideoMode->BitsPerPixel#} ptr
    return (VideoMode width height bpp)

{#fun VideoMode_GetDesktopModeWrapper as videoModeGetDesktopMode
 {} -> `VideoMode' peek* #}

withVideoMode = with

{#fun VideoMode_IsValidWrapper as videoModeIsValid
 {withVideoMode* `VideoMode'} -> `Bool' #}

{#fun VideoMode_GetFullscreenModes as videoModeGetFullscreenModes_
 {alloca- `CULong' peek*} -> `Ptr VideoMode' id #}

videoModeGetFullscreenModes :: IO [VideoMode]
videoModeGetFullscreenModes = do
  (ptrs, size) <- videoModeGetFullscreenModes_
  peekArray (fromIntegral size) ptrs

-- videoModeWidth :: VideoMode -> IO Int
-- videoModeWidth (VideoMode vm) = withForeignPtr vm $ \ptr -> fmap fromIntegral $ {#get VideoMode->Width#} ptr

-- videoModeHeight :: VideoMode -> IO Int
-- videoModeHeight (VideoMode vm) = withForeignPtr vm $ \ptr -> fmap fromIntegral $ {#get VideoMode->Height#} ptr

-- videoModeBitsPerPixel :: VideoMode -> IO Int
-- videoModeBitsPerPixel (VideoMode vm) = withForeignPtr vm $ \ptr -> fmap fromIntegral $ {#get VideoMode->BitsPerPixel#} ptr

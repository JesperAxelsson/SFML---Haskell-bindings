{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Window.Internal.Window where

#include <SFML/Window/Window.h>
#include "WindowWrapper.c"

{#import SFML.Window.Internal.Types #}
{#import SFML.Window.Internal.Event #}
{#import SFML.Window.Internal.VideoMode #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.IORef

{#context lib="csfml-window" prefix="sf" #}

instance Storable ContextSettings where
  sizeOf _ = {#sizeof ContextSettings #}
  alignment _ = {#alignof ContextSettings #}
  peek ptr = do
    depthBits <- fmap fromIntegral $ {#get ContextSettings->DepthBits#} ptr
    stencilBits <- fmap fromIntegral $ {#get ContextSettings->StencilBits#} ptr
    antialiasingLevel <- fmap fromIntegral $ {#get ContextSettings->AntialiasingLevel#} ptr
    majorVersion <- fmap fromIntegral $ {#get ContextSettings->MajorVersion#} ptr
    minorVersion <- fmap fromIntegral $ {#get ContextSettings->MinorVersion#} ptr
    return (ContextSettings depthBits stencilBits antialiasingLevel majorVersion minorVersion)
  poke ptr (ContextSettings depthBits stencilBits antialiasingLevel majorVersion minorVersion) = do
    {#set ContextSettings->DepthBits#} ptr (fromIntegral depthBits)
    {#set ContextSettings->StencilBits#} ptr (fromIntegral stencilBits)
    {#set ContextSettings->AntialiasingLevel#} ptr (fromIntegral antialiasingLevel)
    {#set ContextSettings->MajorVersion#} ptr (fromIntegral majorVersion)
    {#set ContextSettings->MinorVersion#} ptr (fromIntegral minorVersion)

foreign import ccall unsafe "&sfWindow_Destroy"
  windowDestroy :: FinalizerPtr Window

mkWindow :: Ptr Window -> IO Window
mkWindow ptr = fmap Window $ newForeignPtr windowDestroy ptr

{#fun unsafe Window_CreateWrapper as windowCreate
 {withT* `VideoMode'
 ,`String'
 ,stylesToCULong `[Style]'
 ,'withMaybe withT'* `Maybe ContextSettings'} -> `Window' mkWindow* #}

{#fun unsafe Window_Close as ^
 {withWindow* `Window'} -> `()' #}

{#fun unsafe Window_IsOpened as ^
 {withWindow* `Window'} -> `Bool' #}

{#fun unsafe Window_GetWidth as ^
 {withWindow* `Window'} -> `Word' fromIntegral #}

{#fun unsafe Window_GetHeight as ^
 {withWindow* `Window'} -> `Word' fromIntegral #}

{#fun unsafe Window_GetSettingsWrapper as windowGetSettings
 {withWindow* `Window', alloca- `ContextSettings' peek*} -> `()' #}

windowGetEvent :: Window -> IO (Maybe Event)
windowGetEvent window =
  withWindow window $ \w ->
  alloca $ \evtPtr ->
  {#call unsafe Window_GetEvent#} w evtPtr >>= \b ->
  if toBool b then do
    evt <- peek evtPtr
    return (Just evt)
  else
    return Nothing

windowWaitEvent :: Window -> IO (Maybe Event)
windowWaitEvent window =
  withWindow window $ \w ->
  alloca $ \evtPtr ->
  {#call unsafe Window_WaitEvent #} w evtPtr >>= \b ->
  if toBool b then do
    evt <- peek evtPtr
    return (Just evt)
  else
    return Nothing

{#fun unsafe Window_EnableVerticalSync as ^
 {withWindow* `Window' ,`Bool'} -> `()' #}

{#fun unsafe Window_ShowMouseCursor as ^
 {withWindow* `Window', `Bool'} -> `()' #}

{#fun unsafe Window_SetCursorPosition as ^
 {withWindow* `Window', fromIntegral `Word', fromIntegral `Word'} -> `()' #}

{#fun unsafe Window_SetPosition as ^
 {withWindow* `Window', `Int', `Int'} -> `()' #}

{#fun unsafe Window_SetSize as ^
 {withWindow* `Window', fromIntegral `Word', fromIntegral `Word'} -> `()' #}

{#fun unsafe Window_Show as ^
 {withWindow* `Window', `Bool'} -> `()' #}

{#fun unsafe Window_EnableKeyRepeat as ^
 {withWindow* `Window', `Bool'} -> `()' #}

{#fun unsafe Window_SetIcon as ^
 {withWindow* `Window'
 ,fromIntegral `Word'
 ,fromIntegral `Word'
 ,'withByteString (undefined :: CUChar)'* `ByteString'} -> `()' #}

{#fun unsafe Window_SetActive as ^
 {withWindow* `Window'
 ,`Bool'} -> `Bool' #}

{#fun unsafe Window_Display as ^
 {withWindow* `Window'} -> `()' #}

{#fun unsafe Window_GetInput as ^
 {withWindow* `Window'} -> `Input' id #}

{#fun unsafe Window_SetFramerateLimit as ^
 {withWindow* `Window'
 ,fromIntegral `Word'} -> `()' #}

{#fun unsafe Window_GetFrameTime as ^
 {withWindow* `Window'} -> `Float' #}

{#fun unsafe Window_SetJoystickThreshold as ^
 {withWindow* `Window', `Float'} -> `()' #}

{#fun unsafe Window_GetSystemHandle as ^
 {withWindow* `Window'} -> `WindowHandle' id #}

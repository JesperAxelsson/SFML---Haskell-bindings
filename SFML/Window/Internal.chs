{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SFML.Window.Internal where

#include <SFML/Window.h>
#include "VideoModeWrapper.c"
#include "WindowWrapper.c"

import SFML.ForeignUtils
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Word
import Data.Bits (Bits, (.|.))
import Data.List (foldl')
import Data.Char (chr, ord)
import Data.ByteString (ByteString)
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

data Event = Closed
           | Resized { width, height :: Word }
           | LostFocus
           | GainedFocus
           | TextEntered {char :: Char}
           | KeyPressed { code :: KeyCode
                        , alt, control, shift :: Bool
                        }
           | KeyReleased { code :: KeyCode
                         , alt, control, shift :: Bool
                         }
           | MouseWheelMoved { delta, x, y :: Int }
           | MouseButtonPressed { button :: MouseButton
                                , x, y :: Int
                                }
           | MouseButtonReleased { button :: MouseButton
                                 , x, y :: Int
                                 }
           | MouseMoved {x, y :: Int}
           | MouseEntered
           | MouseLeft
           | JoyButtonPressed { joyId, joyButton :: Word }
           | JoyButtonReleased { joyId, joyButton :: Word }
           | JoyMoved { joyId :: Word
                      , axis :: JoyAxis
                      , movePosition :: Float
                      }
           deriving (Show, Eq)

{#pointer *Event as EventPtr -> Event #}

instance Storable Event where
  sizeOf _ = {#sizeof Event #}
  alignment _ = {#alignof Event #}
  peek ptr = do
    t <- fmap cToEnum $ {#get Event.Type #} ptr
    case t of
      EvtClosed -> return Closed
      EvtResized -> peekSizeEvent ptr
      EvtLostFocus -> return LostFocus
      EvtGainedFocus -> return GainedFocus
      EvtTextEntered -> peekTextEvent ptr
      EvtKeyPressed -> peekKeyEvent KeyPressed ptr
      EvtKeyReleased -> peekKeyEvent KeyReleased ptr
      EvtMouseWheelMoved -> peekMouseWheelEvent ptr
      EvtMouseButtonPressed -> peekMouseButtonEvent MouseButtonPressed ptr
      EvtMouseButtonReleased -> peekMouseButtonEvent MouseButtonReleased ptr
      EvtMouseMoved -> peekMouseMoveEvent ptr
      EvtMouseEntered -> return MouseEntered
      EvtMouseLeft -> return MouseLeft
      EvtJoyButtonPressed -> peekJoyButtonEvent JoyButtonPressed ptr
      EvtJoyButtonReleased -> peekJoyButtonEvent JoyButtonReleased ptr
      EvtJoyMoved -> peekJoyMoveEvent ptr
  poke ptr evt = case evt of
    Closed{} -> pokeEvt ptr EvtClosed
    Resized{} -> pokeEvt ptr EvtResized >> pokeSizeEvent ptr evt
    LostFocus{} -> pokeEvt ptr EvtLostFocus
    GainedFocus{} -> pokeEvt ptr EvtGainedFocus
    TextEntered{} -> pokeEvt ptr EvtTextEntered >> pokeTextEvent ptr evt
    KeyPressed{} -> pokeEvt ptr EvtKeyPressed >> pokeKeyEvent ptr evt
    KeyReleased{} -> pokeEvt ptr EvtKeyReleased >> pokeKeyEvent ptr evt
    MouseWheelMoved{} -> pokeEvt ptr EvtMouseWheelMoved >> pokeMouseMoveEvent ptr evt
    MouseButtonPressed{} -> pokeEvt ptr EvtMouseButtonPressed >> pokeMouseButtonEvent ptr evt
    MouseButtonReleased{} -> pokeEvt ptr EvtMouseButtonReleased >> pokeMouseButtonEvent ptr evt
    MouseMoved{} -> pokeEvt ptr EvtMouseMoved >> pokeMouseMoveEvent ptr evt
    MouseEntered{} -> pokeEvt ptr EvtMouseEntered
    MouseLeft{} -> pokeEvt ptr EvtMouseLeft
    JoyButtonPressed{} -> pokeEvt ptr EvtJoyButtonPressed >> pokeJoyButtonEvent ptr evt
    JoyButtonReleased{} -> pokeEvt ptr EvtJoyButtonReleased >> pokeJoyButtonEvent ptr evt
    JoyMoved{} -> pokeEvt ptr EvtJoyMoved >> pokeJoyMoveEvent ptr evt

peekEvt ptr = do
  fmap cToEnum $ {#get Event.Type #} ptr
pokeEvt ptr evt = {#set Event.Type #} ptr (cFromEnum evt)


peekKeyEvent constr ptr = do
  code <- fmap cToEnum $ {#get Event.Key.Code #} ptr
  alt <- fmap toBool $ {#get Event.Key.Alt #} ptr
  ctrl <- fmap toBool $ {#get Event.Key.Control #} ptr
  shift <- fmap toBool $ {#get Event.Key.Shift #} ptr
  return (constr code alt ctrl shift)
-- (KeyEvent code alt ctrl shift)
pokeKeyEvent ptr evt = do
  {#set Event.Key.Code #} ptr (cFromEnum (code evt))
  {#set Event.Key.Alt #} ptr (fromBool (alt evt))
  {#set Event.Key.Control #} ptr (fromBool (control evt))
  {#set Event.Key.Shift #} ptr (fromBool (shift evt))

peekTextEvent ptr = fmap (TextEntered . chr . fromIntegral) $ {#get Event.Text.Unicode #} ptr
pokeTextEvent ptr (TextEntered char) = {#set Event.Text.Unicode #} ptr (fromIntegral . ord $ char)

peekMouseMoveEvent ptr = do
  x <- fmap fromIntegral $ {#get Event.MouseMove.X #} ptr
  y <- fmap fromIntegral $ {#get Event.MouseMove.Y #} ptr
  return (MouseMoved x y)
pokeMouseMoveEvent ptr (MouseMoved x y) = do
  {#set Event.MouseMove.X #} ptr (fromIntegral x)
  {#set Event.MouseMove.Y #} ptr (fromIntegral y)

peekMouseButtonEvent constr ptr = do
  b <- fmap cToEnum $ {#get Event.MouseButton.Button #} ptr
  x <- fmap fromIntegral $ {#get Event.MouseButton.X #} ptr
  y <- fmap fromIntegral $ {#get Event.MouseButton.Y #} ptr
  return (constr b x y)
-- (MouseButtonEvent b x y)
pokeMouseButtonEvent ptr evt = do
  {#set Event.MouseButton.Button #} ptr (cFromEnum (button evt))
  {#set Event.MouseButton.X #} ptr (fromIntegral (x evt))
  {#set Event.MouseButton.Y #} ptr (fromIntegral (y evt))

peekMouseWheelEvent ptr = do
  d <- fmap fromIntegral $ {#get Event.MouseWheel.Delta #} ptr
  x <- fmap fromIntegral $ {#get Event.MouseWheel.X #} ptr
  y <- fmap fromIntegral $ {#get Event.MouseWheel.Y #} ptr
  return (MouseWheelMoved d x y)
pokeMouseWheelEvent ptr (MouseWheelMoved d x y) = do
  {#set Event.MouseWheel.Delta #} ptr (fromIntegral d)
  {#set Event.MouseWheel.X #} ptr (fromIntegral x)
  {#set Event.MouseWheel.Y #} ptr (fromIntegral y)

peekJoyMoveEvent ptr = do
  id <- fmap fromIntegral $ {#get Event.JoyMove.JoystickId #} ptr
  a <- fmap cToEnum $ {#get Event.JoyMove.Axis #} ptr
  p <- fmap realToFrac $ {#get Event.JoyMove.Position #} ptr
  return (JoyMoved id a p)
pokeJoyMoveEvent ptr (JoyMoved id a p) = do
  {#set Event.JoyMove.JoystickId #} ptr (fromIntegral id)
  {#set Event.JoyMove.Axis #} ptr (cFromEnum a)
  {#set Event.JoyMove.Position #} ptr (realToFrac p)

peekJoyButtonEvent constr ptr = do
  id <- fmap fromIntegral $ {#get Event.JoyButton.JoystickId #} ptr
  b <- fmap fromIntegral $ {#get Event.JoyButton.Button #} ptr
  return (constr id b)
-- (JoyMoveEvent id b)
pokeJoyButtonEvent ptr evt = do
  {#set Event.JoyButton.JoystickId #} ptr (fromIntegral (joyId evt))
  {#set Event.JoyButton.Button #} ptr (fromIntegral (joyButton evt))

peekSizeEvent ptr = do
  w <- fmap fromIntegral $ {#get Event.Size.Width #} ptr
  h <- fmap fromIntegral $ {#get Event.Size.Height #} ptr
  return (Resized w h)
pokeSizeEvent ptr (Resized w h) = do
  {#set Event.Size.Width #} ptr (fromIntegral w)
  {#set Event.Size.Height #} ptr (fromIntegral h)
  
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

{------------------------------------  Window  ------------------------------------}
#c
typedef enum
{
  None = sfNone,
  Titlebar = sfTitlebar,
  Resize = sfResize,
  Close = sfClose,
  Fullscreen = sfFullscreen,
  DefaultStyle = sfDefaultStyle
} Style;
#endc
{#enum Style {} with prefix = "" deriving (Eq, Show) #}

stylesToCULong :: [Style] -> CULong
stylesToCULong styles = foldl' ((. fromIntegral . fromEnum) . (.|.)) 0 styles

data ContextSettings = ContextSettings {
    contextSettingDepthBits
  , contextSettingStencilBits
  , contextSettingAntialiasingLevel
  , contextSettingMajorVersion
  , contextSettingMinorVersion :: Word
  } deriving (Eq, Show)

{#pointer *ContextSettings as ContextSettingsPtr -> ContextSettings #}

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

{#pointer *Window foreign newtype #}

foreign import ccall unsafe "&sfWindow_Destroy"
  windowDestroy :: FinalizerPtr Window

mkWindow :: Ptr Window -> IO Window
mkWindow win = do
  win' <- newForeignPtr windowDestroy win
  return (Window win')

{#fun unsafe Window_CreateWrapper as windowCreate
 {withT* `VideoMode'
 ,`String'
 ,stylesToCULong `[Style]'
 ,withT* `ContextSettings'} -> `Window' mkWindow* #}

{#fun unsafe Window_CreateWrapperSimple as windowCreateSimple
 {withT* `VideoMode'
 ,`String'
 ,stylesToCULong `[Style]'} -> `Window' mkWindow* #}

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
 ,withByteString* `ByteString'} -> `()' #}

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

 
{--------  End chs (autogenerated foreign imports in .hs file start here)  --------}

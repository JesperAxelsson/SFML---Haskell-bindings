{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Window.Internal.Types where

#include <SFML/Window/Types.h>
#include <SFML/Window/Window.h>

import SFML.ForeignUtils
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word
import Data.IORef

{#context lib="csfml-window" prefix="sf" #}

{#pointer *Context foreign newtype #}

{#enum KeyCode {} deriving (Eq, Show) #}

{#enum MouseButton {} deriving (Eq, Show) #}

{#enum JoyAxis {} deriving (Eq, Show) #}

{#enum EventType {} deriving (Eq, Show) #}

{#pointer *Event as EventPtr -> Event #}

{#pointer *Input newtype #}

{#pointer *VideoMode as VideoModePtr -> VideoMode #}

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
stylesToCULong [] = enumsToCULong [DefaultStyle]
stylesToCULong styles = enumsToCULong styles

{#pointer *ContextSettings as ContextSettingsPtr -> ContextSettings #}

{#pointer *Window foreign newtype #}

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

data VideoMode = VideoMode {videoModeWidth, videoModeHeight, videoModeBitsPerPixel :: Word }
               deriving (Eq, Show)

type WindowHandle = {#type WindowHandle #}

data ContextSettings = ContextSettings {
    contextSettingDepthBits
  , contextSettingStencilBits
  , contextSettingAntialiasingLevel
  , contextSettingMajorVersion
  , contextSettingMinorVersion :: Word
  } deriving (Eq, Show)

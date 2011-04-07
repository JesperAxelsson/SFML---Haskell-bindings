{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Window.Internal.Input where

#include <SFML/Window/Input.h>

{#import SFML.Window.Internal.Types #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Data.Word

{#context lib="csfml-window" prefix="sf" #}

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

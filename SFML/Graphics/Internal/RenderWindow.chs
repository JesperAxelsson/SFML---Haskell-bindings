{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.RenderWindow where

#include <SFML/Graphics/RenderWindow.h>
#include "RenderWindowWrapper.c"

{#import SFML.Window.Internal.Types #}
{#import SFML.Window.Internal.Event #}
{#import SFML.Window.Internal.VideoMode #}
{#import SFML.Window.Internal.Context #}
{#import SFML.Window.Internal.Window #}
{#import SFML.Graphics.Internal.Types #}
{#import SFML.Graphics.Internal.Rect #}
{#import SFML.Graphics.Internal.View #}
{#import SFML.Graphics.Internal.Color #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.Storable
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word
import Data.ByteString (ByteString)

{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall unsafe "&sfRenderWindow_Destroy"
  renderWindowDestroy :: FinalizerPtr RenderWindow

mkRenderWindow :: Ptr RenderWindow -> IO RenderWindow
mkRenderWindow ptr = fmap RenderWindow $ newForeignPtr renderWindowDestroy ptr

{#fun unsafe RenderWindow_CreateWrapper as renderWindowCreate
 {withT* `VideoMode'
 ,`String'
 ,stylesToCULong `[Style]'
 ,'withMaybe withT'* `Maybe ContextSettings'} -> `RenderWindow' mkRenderWindow* #}

{#fun unsafe RenderWindow_CreateFromHandle as ^
 {id `WindowHandle'
 ,withT* `ContextSettings'} -> `RenderWindow' mkRenderWindow* #}

{#fun unsafe RenderWindow_Close as ^
 {withRenderWindow* `RenderWindow'} -> `()' #}

{#fun unsafe RenderWindow_IsOpened as ^
 {withRenderWindow* `RenderWindow'} -> `Bool' #}

{#fun unsafe RenderWindow_GetWidth as ^
 {withRenderWindow* `RenderWindow'} -> `Word' fromIntegral #}

{#fun unsafe RenderWindow_GetHeight as ^
 {withRenderWindow* `RenderWindow'} -> `Word' fromIntegral #}

{#fun unsafe RenderWindow_GetSettingsWrapper as renderWindowGetSettings
 {withRenderWindow* `RenderWindow'
 ,alloca- `ContextSettings' peek*} -> `()' #}

renderWindowGetEvent :: RenderWindow -> IO (Maybe Event)
renderWindowGetEvent window =
  withRenderWindow window $ \winPtr ->
  alloca $ \evtPtr ->
  {#call unsafe RenderWindow_GetEvent#} winPtr evtPtr >>= \b ->
  if toBool b then do
    evt <- peek evtPtr
    return (Just evt)
  else return Nothing

renderWindowWaitEvent :: RenderWindow -> IO (Maybe Event)
renderWindowWaitEvent window =
  withRenderWindow window $ \winPtr ->
  alloca $ \evtPtr ->
  {#call unsafe RenderWindow_WaitEvent#} winPtr evtPtr >>= \b ->
  if toBool b then do
    evt <- peek evtPtr
    return (Just evt)
  else return Nothing
       
{#fun unsafe RenderWindow_EnableVerticalSync as ^
 {withRenderWindow* `RenderWindow'
 ,`Bool'} -> `()' #}

{#fun unsafe RenderWindow_ShowMouseCursor as ^
 {withRenderWindow* `RenderWindow'
 ,`Bool'} -> `()' #}

{#fun unsafe RenderWindow_SetCursorPosition as ^
 {withRenderWindow* `RenderWindow'
 ,fromIntegral `Word'
 ,fromIntegral `Word'} -> `()' #}

{#fun unsafe RenderWindow_SetPosition as ^
 {withRenderWindow* `RenderWindow'
 ,`Int'
 ,`Int'} -> `()' #}

{#fun unsafe RenderWindow_SetSize as ^
 {withRenderWindow* `RenderWindow'
 ,fromIntegral `Word'
 ,fromIntegral `Word'} -> `()' #}

{#fun unsafe RenderWindow_Show as ^
 {withRenderWindow* `RenderWindow'
 ,`Bool'} -> `()' #}

{#fun unsafe RenderWindow_EnableKeyRepeat as ^
 {withRenderWindow* `RenderWindow'
 ,`Bool'} -> `()' #}

{#fun unsafe RenderWindow_SetIcon as ^
 {withRenderWindow* `RenderWindow'
 ,fromIntegral `Word'
 ,fromIntegral `Word'
 ,withByteString* `ByteString'} -> `()' #}

{#fun unsafe RenderWindow_SetActive as ^
 {withRenderWindow* `RenderWindow'
 ,`Bool'} -> `Bool' #}

{#fun unsafe RenderWindow_SaveGLStates as ^
 {withRenderWindow* `RenderWindow'} -> `()' #}

{#fun unsafe RenderWindow_RestoreGLStates as ^
 {withRenderWindow* `RenderWindow'} -> `()' #}

{#fun unsafe RenderWindow_Display as ^
 {withRenderWindow* `RenderWindow'} -> `()' #}

{#fun unsafe RenderWindow_GetInput as ^
 {withRenderWindow* `RenderWindow'} -> `Input' id #}

{#fun unsafe RenderWindow_SetFramerateLimit as ^
 {withRenderWindow* `RenderWindow'
 ,fromIntegral `Word'} -> `()' #}

{#fun unsafe RenderWindow_GetFrameTime as ^
 {withRenderWindow* `RenderWindow'} -> `Float' #}

{#fun unsafe RenderWindow_SetJoystickThreshold as ^
 {withRenderWindow* `RenderWindow'
 ,`Float'} -> `()' #}

{#fun unsafe RenderWindow_GetSystemHandle as ^
 {withRenderWindow* `RenderWindow'} -> `WindowHandle' id #}

{#fun unsafe RenderWindow_DrawSprite as ^
 {withRenderWindow* `RenderWindow'
 ,withSprite* `Sprite'} -> `()' #}

{#fun unsafe RenderWindow_DrawShape as ^
 {withRenderWindow* `RenderWindow'
 ,withShape* `Shape'} -> `()' #}

{#fun unsafe RenderWindow_DrawText as ^
 {withRenderWindow* `RenderWindow'
 ,withText* `Text'} -> `()' #}

{#fun unsafe RenderWindow_DrawSpriteWithShader as ^
 {withRenderWindow* `RenderWindow'
 ,withSprite* `Sprite'
 ,withShader* `Shader'} -> `()' #}

{#fun unsafe RenderWindow_DrawShapeWithShader as ^
 {withRenderWindow* `RenderWindow'
 ,withShape* `Shape'
 ,withShader* `Shader'} -> `()' #}

{#fun unsafe RenderWindow_DrawTextWithShader as ^
 {withRenderWindow* `RenderWindow'
 ,withText* `Text'
 ,withShader* `Shader'} -> `()' #}

{#fun unsafe RenderWindow_ClearWrapper as renderWindowClear
 {withRenderWindow* `RenderWindow'
 ,withT* `Color'} -> `()' #}

{#fun unsafe RenderWindow_SetView as ^
 {withRenderWindow* `RenderWindow'
 ,withView* `View'} -> `()' #}

{#fun unsafe RenderWindow_GetView as ^
 {withRenderWindow* `RenderWindow'} -> `View' mkConstView* #}

{#fun unsafe RenderWindow_GetDefaultView as ^
 {withRenderWindow* `RenderWindow'} -> `View' mkConstView* #}

{#fun unsafe RenderWindow_GetViewportWrapper as renderWindowGetViewport
 {withRenderWindow* `RenderWindow'
 ,withView* `View'
 ,allocaIntRect- `Rect Int' peekRect*} -> `()' #}

{#fun unsafe RenderWindow_ConvertCoords as renderWindowConvertCoords_
 {withRenderWindow* `RenderWindow'
 ,fromIntegral `Word'
 ,fromIntegral `Word'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*
 ,'withMaybe withView'* `Maybe View'} -> `()'#}

renderWindowConvertCoords :: RenderWindow -> Word -> Word -> Maybe View -> IO (Float, Float)
renderWindowConvertCoords window x y view = do
  (x', y') <- renderWindowConvertCoords_ window x y view
  return (realToFrac x', realToFrac y')

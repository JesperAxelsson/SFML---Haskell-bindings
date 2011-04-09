{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.RenderImage where

#include <SFML/Graphics/RenderImage.h>
#include "RenderImageWrapper.c"

{#import SFML.Graphics.Internal.Types #}
{#import SFML.Graphics.Internal.Rect #}
{#import SFML.Graphics.Internal.View #}
{#import SFML.Graphics.Internal.Color #}
{#import SFML.Graphics.Internal.Image #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.Storable
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word



{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall unsafe "&sfRenderImage_Destroy"
  renderImageDestroy :: FinalizerPtr RenderImage

mkRenderImage :: Ptr RenderImage -> IO RenderImage
mkRenderImage ptr = fmap RenderImage $ newForeignPtr renderImageDestroy ptr

{#fun unsafe RenderImage_Create as ^
 {fromIntegral `Word'
 ,fromIntegral `Word'
 ,`Bool'} -> `RenderImage' mkRenderImage* #}

{#fun unsafe RenderImage_GetWidth as ^
 {withRenderImage* `RenderImage'} -> `Word' fromIntegral #}

{#fun unsafe RenderImage_GetHeight as ^
 {withRenderImage* `RenderImage'} -> `Word' fromIntegral #}

{#fun unsafe RenderImage_SetActive as ^
 {withRenderImage* `RenderImage'
 ,`Bool'} -> `Bool' #}

{#fun unsafe RenderImage_SaveGLStates as ^
 {withRenderImage* `RenderImage'} -> `()' #}

{#fun unsafe RenderImage_RestoreGLStates as ^
 {withRenderImage* `RenderImage'} -> `()' #}

{#fun unsafe RenderImage_Display as ^
 {withRenderImage* `RenderImage'} -> `()' #}

{#fun unsafe RenderImage_DrawSprite as ^
 {withRenderImage* `RenderImage'
 ,withSprite* `Sprite'} -> `()' #}

{#fun unsafe RenderImage_DrawShape as ^
 {withRenderImage* `RenderImage'
 ,withShape* `Shape'} -> `()' #}

{#fun unsafe RenderImage_DrawText as ^
 {withRenderImage* `RenderImage'
 ,withText* `Text'} -> `()' #}

{#fun unsafe RenderImage_DrawSpriteWithShader as ^
 {withRenderImage* `RenderImage'
 ,withSprite* `Sprite'
 ,withShader* `Shader'} -> `()' #}

{#fun unsafe RenderImage_DrawShapeWithShader as ^
 {withRenderImage* `RenderImage'
 ,withShape* `Shape'
 ,withShader* `Shader'} -> `()' #}

{#fun unsafe RenderImage_DrawTextWithShader as ^
 {withRenderImage* `RenderImage'
 ,withText* `Text'
 ,withShader* `Shader'} -> `()' #}

{#fun unsafe RenderImage_ClearWrapper as renderImageClear
 {withRenderImage* `RenderImage'
 ,withT* `Color'} -> `()' #}

{#fun unsafe RenderImage_SetView as ^
 {withRenderImage* `RenderImage'
 ,withView* `View'} -> `()' #}

{#fun unsafe RenderImage_GetView as ^
 {withRenderImage* `RenderImage'} -> `View' mkConstView* #}

{#fun unsafe RenderImage_GetDefaultView as ^
 {withRenderImage* `RenderImage'} -> `View' mkConstView* #}

{#fun unsafe RenderImage_GetViewportWrapper as renderImageGetViewport
 {withRenderImage* `RenderImage'
 ,withView* `View'
 ,allocaIntRect- `Rect Int' peekRect*} -> `()' #}

{#fun unsafe RenderImage_ConvertCoords as renderImageConvertCoords_
 {withRenderImage* `RenderImage'
 ,fromIntegral `Word'
 ,fromIntegral `Word'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*
 ,withView* `View'} -> `()'#}

renderImageConvertCoords :: RenderImage -> Word -> Word -> View -> IO (Float, Float)
renderImageConvertCoords image x y view = do
  (x', y') <- renderImageConvertCoords_ image x y view
  return (realToFrac x', realToFrac y')

{#fun unsafe RenderImage_GetImage as ^
 {withRenderImage* `RenderImage'} -> `Image' mkConstImage* #}

{#fun pure unsafe RenderImage_IsAvailable as ^
 {} -> `Bool' toBool #}

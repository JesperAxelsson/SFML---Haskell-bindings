{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.View where

#include <SFML/Graphics/View.h>
#include "ViewWrapper.c"

{#import SFML.Graphics.Internal.Types #}
{#import SFML.Graphics.Internal.Rect #}
import SFML.ForeignUtils
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C

{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall unsafe "&sfView_Destroy"
  viewDestroy :: FinalizerPtr View

mkView :: Ptr View -> IO View
mkView ptr = fmap View $ newForeignPtr viewDestroy ptr

mkConstView :: Ptr View -> IO View
mkConstView ptr = fmap View $ newForeignPtr_ ptr

{#fun unsafe View_Create as ^
 {} -> `View' mkView* #}

{#fun unsafe View_CreateFromRectWrapper as viewCreateFromRect
 {allocaFloatRect- `Rect Float' peekRect*} -> `View' mkView* #}

{#fun unsafe View_Copy as ^
 {withView* `View'} -> `View' mkView* #}

{#fun unsafe View_SetCenter as ^
 {withView* `View'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe View_SetSize as ^
 {withView* `View'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe View_SetRotation as ^
 {withView* `View'
 ,`Float'} -> `()' #}

{#fun unsafe View_SetViewportWrapper as viewSetViewport
 {withView* `View'
 ,castWithT* `Rect Float'} -> `()' #}

{#fun unsafe View_ResetWrapper as viewReset
 {withView* `View'
 ,castWithT* `Rect Float'} -> `()' #}

{#fun unsafe View_GetCenterX as ^
 {withView* `View'} -> `Float' #}

{#fun unsafe View_GetCenterY as ^
 {withView* `View'} -> `Float' #}

{#fun unsafe View_GetWidth as ^
 {withView* `View'} -> `Float' #}

{#fun unsafe View_GetHeight as ^
 {withView* `View'} -> `Float' #}

{#fun unsafe View_GetRotation as ^
 {withView* `View'} -> `Float' #}

{#fun unsafe View_GetViewportWrapper as viewGetViewport
 {withView* `View'
 ,allocaFloatRect- `Rect Float' peekRect*} -> `()' #}

{#fun unsafe View_Move as ^
 {withView* `View'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe View_Rotate as ^
 {withView* `View'
 ,`Float'} -> `()' #}

{#fun unsafe View_Zoom as ^
 {withView* `View'
 ,`Float'} -> `()' #}

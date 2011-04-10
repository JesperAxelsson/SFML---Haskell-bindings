{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.Shape where

#include <SFML/Graphics/Shape.h>
#include "ShapeWrapper.c"

{#import SFML.Graphics.Internal.Types #}
{#import SFML.Graphics.Internal.Color #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word

{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall unsafe "&sfShape_Destroy"
  shapeDestroy :: FinalizerPtr Shape
                  
mkShape :: Ptr Shape -> IO Shape
mkShape ptr = fmap Shape $ newForeignPtr shapeDestroy ptr

{#fun unsafe Shape_Create as ^
 {} -> `Maybe Shape' 'fromNull mkShape'* #}

{#fun unsafe Shape_CreateLineWrapper as shapeCreateLine
 {`Float'
 ,`Float'
 ,`Float'
 ,`Float'
 ,`Float'
 ,withT* `Color'
 ,`Float'
 ,withT* `Color'} -> `Shape' mkShape* #}

{#fun unsafe Shape_CreateRectangleWrapper as shapeCreateRectangle
 {`Float'
 ,`Float'
 ,`Float'
 ,`Float'
 ,withT* `Color'
 ,`Float'
 ,withT* `Color'} -> `Shape' mkShape* #}

{#fun unsafe Shape_CreateCircleWrapper as shapeCreateCircle
 {`Float'
 ,`Float'
 ,`Float'
 ,withT* `Color'
 ,`Float'
 ,withT* `Color'} -> `Shape' mkShape* #}

{#fun unsafe Shape_Copy as ^
 {withShape* `Shape'} -> `Shape' mkShape* #}

{#fun unsafe Shape_SetX as ^
 {withShape* `Shape'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_SetY as ^
 {withShape* `Shape'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_SetPosition as ^
 {withShape* `Shape'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_SetScaleX as ^
 {withShape* `Shape'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_SetScaleY as ^
 {withShape* `Shape'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_SetScale as ^
 {withShape* `Shape'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_SetRotation as ^
 {withShape* `Shape'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_SetOrigin as ^
 {withShape* `Shape'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_SetColorWrapper as shapeSetColor
 {withShape* `Shape'
 ,withT* `Color'} -> `()' #}

{#fun unsafe Shape_SetBlendMode as ^
 {withShape* `Shape'
 ,cFromEnum `BlendMode'} -> `()' #}

{#fun unsafe Shape_GetX as ^
 {withShape* `Shape'} -> `Float' #}

{#fun unsafe Shape_GetY as ^
 {withShape* `Shape'} -> `Float' #}

{#fun unsafe Shape_GetScaleX as ^
 {withShape* `Shape'} -> `Float' #}

{#fun unsafe Shape_GetScaleY as ^
 {withShape* `Shape'} -> `Float' #}

{#fun unsafe Shape_GetRotation as ^
 {withShape* `Shape'} -> `Float' #}

{#fun unsafe Shape_GetOriginX as ^
 {withShape* `Shape'} -> `Float' #}

{#fun unsafe Shape_GetOriginY as ^
 {withShape* `Shape'} -> `Float' #}

{#fun unsafe Shape_GetColorWrapper as shapeGetColor
 {withShape* `Shape'
 ,alloca- `Color' peek*} -> `()' #}

{#fun unsafe Shape_GetBlendMode as ^
 {withShape* `Shape'} -> `BlendMode' cToEnum #}

{#fun unsafe Shape_Move as ^
 {withShape* `Shape'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_Scale as ^
 {withShape* `Shape'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_Rotate as ^
 {withShape* `Shape'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_TransformToLocal as shapeTransformToLocal_
 {withShape* `Shape'
 ,`Float'
 ,`Float'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

shapeTransformToLocal :: Shape -> Float -> Float -> IO (Float, Float)
shapeTransformToLocal shape x y = do
  (x', y') <- shapeTransformToLocal_ shape x y
  return (realToFrac x', realToFrac y')

{#fun unsafe Shape_TransformToGlobal as shapeTransformToGlobal_
 {withShape* `Shape'
 ,`Float'
 ,`Float'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

shapeTransformToGlobal :: Shape -> Float -> Float -> IO (Float, Float)
shapeTransformToGlobal shape x y = do
  (x', y') <- shapeTransformToGlobal_ shape x y
  return (realToFrac x', realToFrac y')

{#fun unsafe Shape_AddPointWrapper as shapeAddPoint
 {withShape* `Shape'
 ,`Float'
 ,`Float'
 ,withT* `Color'
 ,withT* `Color'} -> `()' #}

{#fun unsafe Shape_EnableFill as ^
 {withShape* `Shape'
 ,`Bool'} -> `()' #}

{#fun unsafe Shape_EnableOutline as ^
 {withShape* `Shape'
 ,`Bool'} -> `()' #}

{#fun unsafe Shape_SetOutlineThickness as ^
 {withShape* `Shape'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_GetOutlineThickness as ^
 {withShape* `Shape'} -> `Float' #}

{#fun unsafe Shape_GetPointsCount as ^
 {withShape* `Shape'} -> `Word' fromIntegral #}

{#fun unsafe Shape_GetPointPosition as shapeGetPointPosition_
 {withShape* `Shape'
 ,`Int'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

shapeGetPointPosition :: Shape -> Int -> IO (Float, Float)
shapeGetPointPosition shape index = do
  (x, y) <- shapeGetPointPosition_ shape index
  return (realToFrac x, realToFrac y)

{#fun unsafe Shape_GetPointColorWrapper as shapeGetPointColor
 {withShape* `Shape'
 ,fromIntegral `Word'
 ,alloca- `Color' peek*} -> `()' #}

{#fun unsafe Shape_GetPointOutlineColorWrapper as shapeGetPointOutlineColor
 {withShape* `Shape'
 ,fromIntegral `Word'
 ,alloca- `Color' peek*} -> `()' #}

{#fun unsafe Shape_SetPointPosition as ^
 {withShape* `Shape'
 ,fromIntegral `Word'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Shape_SetPointColorWrapper as shapeSetPointColor
 {withShape* `Shape'
 ,fromIntegral `Word'
 ,withT* `Color'} -> `()' #}

{#fun unsafe Shape_SetPointOutlineColorWrapper as shapeSetPointOutlineColor
 {withShape* `Shape'
 ,fromIntegral `Word'
 ,withT* `Color'} -> `()' #}


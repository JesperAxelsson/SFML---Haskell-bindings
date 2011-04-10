{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}

module SFML.Graphics.Internal.Text where

#include <SFML/Graphics/Text.h>
#include "TextWrapper.c"

{#import SFML.Graphics.Internal.Types #}
{#import SFML.Graphics.Internal.Color #}
{#import SFML.Graphics.Internal.Font #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.IORef

{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall unsafe "&sfText_Destroy"
  textDestroy :: FinalizerPtr TextPtr
                 
mkText :: Ptr TextPtr -> IO Text
mkText ptr = do
  textPtr <- fmap TextPtr $ newForeignPtr textDestroy ptr
  textString <- newIORef Nothing
  textFont <- newIORef Nothing
  return (Text textPtr textString textFont)

{#fun unsafe Text_Create as ^
 {} -> `Maybe Text' 'fromNull mkText'* #}

{#fun unsafe Text_Copy as ^
 {withText* `Text'} -> `Text' mkText* #}

{#fun unsafe Text_SetX as ^
 {withText* `Text'
 ,`Float'} -> `()' #}

{#fun unsafe Text_SetY as ^
 {withText* `Text'
 ,`Float'} -> `()' #}

{#fun unsafe Text_SetPosition as ^
 {withText* `Text'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Text_SetScaleX as ^
 {withText* `Text'
 ,`Float'} -> `()' #}

{#fun unsafe Text_SetScaleY as ^
 {withText* `Text'
 ,`Float'} -> `()' #}

{#fun unsafe Text_SetScale as ^
 {withText* `Text'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Text_SetRotation as ^
 {withText* `Text'
 ,`Float'} -> `()' #}

{#fun unsafe Text_SetOrigin as ^
 {withText* `Text'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Text_SetColorWrapper as textSetColor
 {withText* `Text'
 ,withT* `Color'} -> `()' #}

{#fun unsafe Text_SetBlendMode as ^
 {withText* `Text'
 ,cFromEnum `BlendMode' } -> `()' #}

{#fun unsafe Text_GetX as ^
 {withText* `Text'} -> `Float' #}

{#fun unsafe Text_GetY as ^
 {withText* `Text'} -> `Float' #}

{#fun unsafe Text_GetScaleX as ^
 {withText* `Text'} -> `Float' #}

{#fun unsafe Text_GetScaleY as ^
 {withText* `Text'} -> `Float' #}

{#fun unsafe Text_GetRotation as ^
 {withText* `Text'} -> `Float' #}

{#fun unsafe Text_GetOriginX as ^
 {withText* `Text'} -> `Float' #}

{#fun unsafe Text_GetOriginY as ^
 {withText* `Text'} -> `Float' #}

{#fun unsafe Text_GetColorWrapper as textGetColor
 {withText* `Text'
 ,alloca- `Color' peek*} -> `()' #}

{#fun unsafe Text_GetBlendMode as ^
 {withText* `Text'} -> `BlendMode' cToEnum #}

{#fun unsafe Text_Move as ^
 {withText* `Text'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Text_Scale as ^
 {withText* `Text'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Text_Rotate as ^
 {withText* `Text'
 ,`Float'} -> `()' #}

{#fun unsafe Text_TransformToLocal as textTransformToLocal_
 {withText* `Text'
 ,`Float'
 ,`Float'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

textTransformToLocal text x y = do
  (x', y') <- textTransformToLocal_ text x y
  return (realToFrac x', realToFrac y')

{#fun unsafe Text_TransformToGlobal as textTransformToGlobal_
 {withText* `Text'
 ,`Float'
 ,`Float'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

textTransformToGlobal text x y = do
  (x', y') <- textTransformToGlobal_ text x y
  return (realToFrac x', realToFrac y')

{#fun unsafe Text_SetString as textSetString_
 {withTextPtr* `TextPtr'
 ,id `CString'} -> `()' #}

textSetString :: Text -> String -> IO ()
textSetString Text{textPtr, textString} newString = do
  strPtr <- newCString newString
  strFPtr <- newForeignPtr finalizerFree (castPtr strPtr)
  writeIORef textString (Just strFPtr)
  textSetString_ textPtr strPtr

{#fun unsafe Text_SetUnicodeString as textSetUnicodeString_
 {withTextPtr* `TextPtr'
 ,id `Ptr CUInt'} -> `()' #}

textSetUnicodeString :: Text -> String -> IO ()
textSetUnicodeString Text{textPtr, textString} newString = do
  strPtr <- newUnicodeString newString
  strFPtr <- newForeignPtr finalizerFree (castPtr strPtr)
  writeIORef textString (Just strFPtr)
  textSetUnicodeString_ textPtr strPtr

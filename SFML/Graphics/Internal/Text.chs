{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}

module SFML.Graphics.Internal.Text where

#include <SFML/Graphics/Text.h>
#include "TextWrapper.c"

{#import SFML.Graphics.Internal.Types #}
{#import SFML.Graphics.Internal.Color #}
{#import SFML.Graphics.Internal.Font #}
{#import SFML.Graphics.Internal.Rect #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Word
import Data.IORef

{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall unsafe "&sfText_Destroy"
  textDestroy :: FinalizerPtr TextPtr
                 
mkText :: Ptr TextPtr -> IO Text
mkText ptr = do
  textPtr <- fmap TextPtr $ newForeignPtr textDestroy ptr
  textFont <- newIORef Nothing
  return (Text textPtr textFont)

{#fun unsafe Text_Create as ^
 {} -> `Maybe Text' 'fromNull mkText'* #}

{#fun unsafe Text_Copy as textCopy_
 {withTextPtr* `TextPtr'} -> `Ptr TextPtr' id #}

textCopy :: Text -> IO Text
textCopy text = do
  newTextPtr <- textCopy_ (textPtr text)
  newText <- fmap TextPtr $ newForeignPtr textDestroy newTextPtr
  newTextFont <- newIORef =<< readIORef (textFont text) 
  return (Text newText newTextFont)

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

{#fun unsafe Text_SetString as ^
 {withText* `Text'
 ,`String'} -> `()' #}

{#fun unsafe Text_SetUnicodeString as ^
 {withText* `Text'
 ,withUnicodeString* `String'} -> `()' #}

{#fun unsafe Text_SetFont as textSetFont_
 {withTextPtr* `TextPtr'
 ,withFont* `Font'} -> `()' #}

textSetFont :: Text -> Font -> IO ()
textSetFont text font = do
  textSetFont_ (textPtr text) font
  writeIORef (textFont text) (Just font)

{#fun unsafe Text_SetCharacterSize as ^
 {withText* `Text'
 ,fromIntegral `Word'} -> `()' #}

{#fun unsafe Text_SetStyle as ^
 {withText* `Text'
 ,textStylesToCULong `[TextStyle]'} -> `()' #}

{#fun unsafe Text_GetUnicodeString as ^
 {withText* `Text'} -> `String' fromUnicodeString* #}

{#fun unsafe Text_GetString as ^
 {withText* `Text'} -> `String' #}

textGetFont (Text _ font) = readIORef font

{#fun unsafe Text_GetCharacterSize as ^
 {withText* `Text'} -> `Word' fromIntegral #}

{#fun unsafe Text_GetStyle as ^
 {withText* `Text'} -> `[TextStyle]' cuLongToEnums #}

{#fun unsafe Text_GetCharacterPos as textGetCharacterPos_
 {withText* `Text'
 ,fromIntegral `Word'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

textGetCharacterPos :: Text -> Word -> IO (Float, Float)
textGetCharacterPos text index = do
  (x, y) <- textGetCharacterPos_ text index
  return (realToFrac x, realToFrac y)

{#fun unsafe Text_GetRectWrapper as textGetRect
 {withText* `Text'
 ,allocaFloatRect- `Rect Float' peekRect*} -> `()' #}

{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.Sprite where

#include <SFML/Graphics/Sprite.h>
#include "SpriteWrapper.c"

{#import SFML.Graphics.Internal.Types #}
{#import SFML.Graphics.Internal.Rect #}
{#import SFML.Graphics.Internal.Image #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Data.Word

{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall unsafe "&sfSprite_Destroy"
  spriteDestroy :: FinalizerPtr Sprite

mkSprite :: Ptr Sprite -> IO Sprite
mkSprite ptr = fmap Sprite $ newForeignPtr spriteDestroy ptr

mkConstSprite :: Ptr Sprite -> IO Sprite
mkConstSprite ptr = fmap Sprite $ newForeignPtr_ ptr

{#fun unsafe Sprite_Copy as ^
 {withSprite* `Sprite'} -> `Sprite' mkSprite* #}

{#fun unsafe Sprite_SetX as ^
 {withSprite* `Sprite'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_SetY as ^
 {withSprite* `Sprite'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_SetPosition as ^
 {withSprite* `Sprite'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_SetScaleX as ^
 {withSprite* `Sprite'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_SetScaleY as ^
 {withSprite* `Sprite'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_SetScale as ^
 {withSprite* `Sprite'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_SetRotation as ^
 {withSprite* `Sprite'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_SetOrigin as ^
 {withSprite* `Sprite'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_SetColorWrapper as spriteSetColor
 {withSprite* `Sprite'
 ,withT* `Color'} -> `()' #}

{#fun unsafe Sprite_SetBlendMode as ^
 {withSprite* `Sprite'
 ,cFromEnum `BlendMode'} -> `()' #}

{#fun unsafe Sprite_GetX as ^
 {withSprite* `Sprite'} -> `Float' #}

{#fun unsafe Sprite_GetY as ^
 {withSprite* `Sprite'} -> `Float' #}

{#fun unsafe Sprite_GetScaleX as ^
 {withSprite* `Sprite'} -> `Float' #}

{#fun unsafe Sprite_GetScaleY as ^
 {withSprite* `Sprite'} -> `Float' #}

{#fun unsafe Sprite_GetRotation as ^
 {withSprite* `Sprite'} -> `Float' #}

{#fun unsafe Sprite_GetOriginX as ^
 {withSprite* `Sprite'} -> `Float' #}

{#fun unsafe Sprite_GetOriginY as ^
 {withSprite* `Sprite'} -> `Float' #}

{#fun unsafe Sprite_GetColorWrapper as spriteGetColor
 {withSprite* `Sprite'
 ,alloca- `Color' peek*} -> `()' #}

{#fun unsafe Sprite_GetBlendMode as ^
 {withSprite* `Sprite'} -> `BlendMode' cToEnum #}

{#fun unsafe Sprite_Move as ^
 {withSprite* `Sprite'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_Scale as ^
 {withSprite* `Sprite'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_Rotate as ^
 {withSprite* `Sprite'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_TransformToLocal as ^
 {withSprite* `Sprite'
 ,`Float'
 ,`Float'
 ,allocaFloat- `Float' peekFloat*
 ,allocaFloat- `Float' peekFloat*} -> `()' #}

{#fun unsafe Sprite_TransformToGlobal as ^
 {withSprite* `Sprite'
 ,`Float'
 ,`Float'
 ,allocaFloat- `Float' peekFloat*
 ,allocaFloat- `Float' peekFloat*} -> `()' #}

{#fun unsafe Sprite_SetImage as ^
 {withSprite* `Sprite'
 ,withImage* `Image'
 ,`Bool'} -> `()' #}

{#fun unsafe Sprite_SetSubRectWrapper as spriteSetSubRect
 {withSprite* `Sprite'
 ,castWithT* `Rect Int'} -> `()' #}

{#fun unsafe Sprite_Resize as ^
 {withSprite* `Sprite'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Sprite_FlipX as ^
 {withSprite* `Sprite'
 ,`Bool'} -> `()' #}

{#fun unsafe Sprite_FlipY as ^
 {withSprite* `Sprite'
 ,`Bool'} -> `()' #}

{#fun unsafe Sprite_GetImage as ^
 {withSprite* `Sprite'} -> `Image' mkConstImage* #}

{#fun unsafe Sprite_GetSubRectWrapper as spriteGetSubRect
 {withSprite* `Sprite'
 ,allocaIntRect- `Rect Int' peekRect*} -> `()' #}

{#fun unsafe Sprite_GetWidth as ^
 {withSprite* `Sprite'} -> `Float' #}

{#fun unsafe Sprite_GetHeight as ^
 {withSprite* `Sprite'} -> `Float' #}

{#fun unsafe Sprite_GetPixelWrapper as spriteGetPixel
 {withSprite* `Sprite'
 ,fromIntegral `Word'
 ,fromIntegral `Word'
 ,alloca- `Color' peek*} -> `()' #}

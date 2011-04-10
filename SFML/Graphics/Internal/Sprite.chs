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
import Data.IORef

{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall unsafe "&sfSprite_Destroy"
  spriteDestroy :: FinalizerPtr SpritePtr

mkSprite :: Ptr SpritePtr -> IO Sprite
mkSprite ptr = do
  spritePtr <- fmap SpritePtr $ newForeignPtr spriteDestroy ptr
  spriteImage <- newIORef Nothing
  return (Sprite spritePtr spriteImage)

mkConstSprite :: Ptr SpritePtr -> IO Sprite
mkConstSprite ptr = do
  spritePtr <- fmap SpritePtr $ newForeignPtr_ ptr
  spriteImage <- newIORef Nothing
  return (Sprite spritePtr spriteImage)

{#fun unsafe Sprite_Create as ^
 {} -> `Maybe Sprite' 'fromNull mkSprite'* #}

{#fun unsafe Sprite_Copy as spriteCopy_
 {withSpritePtr* `SpritePtr'} -> `Ptr SpritePtr' id #}

spriteCopy :: Sprite -> IO Sprite
spriteCopy sprite = do
  newPtr <- spriteCopy_ (spritePtr sprite)
  newSprite <- fmap SpritePtr $ newForeignPtr spriteDestroy newPtr
  newSpriteImage <- newIORef =<< readIORef (spriteImage sprite)
  return (Sprite newSprite newSpriteImage)

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

{#fun unsafe Sprite_TransformToLocal as spriteTransformToLocal_
 {withSprite* `Sprite'
 ,`Float'
 ,`Float'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

spriteTransformToLocal :: Sprite -> Float -> Float -> IO (Float, Float)
spriteTransformToLocal sprite x y = do
  (x', y') <- spriteTransformToLocal_ sprite x y
  return (realToFrac x', realToFrac y')

{#fun unsafe Sprite_TransformToGlobal as spriteTransformToGlobal_
 {withSprite* `Sprite'
 ,`Float'
 ,`Float'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*} -> `()' #}

spriteTransformToGlobal :: Sprite -> Float -> Float -> IO (Float, Float)
spriteTransformToGlobal sprite x y = do
  (x', y') <- spriteTransformToGlobal_ sprite x y
  return (realToFrac x', realToFrac y')

{#fun unsafe Sprite_SetImage as spriteSetImage_
 {withSpritePtr* `SpritePtr'
 ,withImage* `Image'
 ,`Bool'} -> `()' #}

spriteSetImage :: Sprite -> Image -> Bool -> IO ()
spriteSetImage sprite image adjustSize = do
  spriteSetImage_ (spritePtr sprite) image adjustSize
  writeIORef (spriteImage sprite) (Just image)

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

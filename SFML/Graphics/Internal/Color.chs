{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.Color where

#include <SFML/Graphics/Color.h>
#include "ColorWrapper.c"

{#import SFML.Graphics.Internal.Types #}
import Data.Word
import SFML.ForeignUtils
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.Marshal.Alloc
--import Foreign.Marshal.Utils

{#context lib="csfml-graphics" prefix="sf" #}



instance Storable Color where
  sizeOf _ = {#sizeof Color #}
  alignment _ = {#alignof Color #}
  peek ptr = do
    r <- fmap fromIntegral $ {#get Color->r#} ptr
    g <- fmap fromIntegral $ {#get Color->g#} ptr
    b <- fmap fromIntegral $ {#get Color->b#} ptr
    a <- fmap fromIntegral $ {#get Color->a#} ptr
    return (Color r g b a)
  poke ptr (Color r g b a) = do
    {#set Color->r#} ptr (fromIntegral r)
    {#set Color->g#} ptr (fromIntegral g)
    {#set Color->b#} ptr (fromIntegral b)
    {#set Color->a#} ptr (fromIntegral a)

colorBlack = Color 0 0 0 255
colorWhite = Color 255 255 255 255
colorRed = Color 255 0 0 255
colorGreen = Color 0 255 0 255
colorBlue = Color 0 0 255 255
colorYellow =  Color 255 255 0 255
colorMagenta = Color 255 0 255 255
colorCyan = Color 0 255 255 255

{#fun unsafe Color_FromRGBWrapper as colorFromRGB
 {`Word8', `Word8', `Word8', alloca- `Color' peek*} -> `()'#}

{#fun unsafe Color_FromRGBAWrapper as colorFromRGBA
 {`Word8', `Word8', `Word8', `Word8', alloca- `Color' peek*} -> `()'#}

{#fun unsafe Color_AddWrapper as colorAdd
 {withT* `Color', withT* `Color', alloca- `Color' peek*} -> `()' #}

{#fun unsafe Color_ModulateWrapper as colorModulate
 {withT* `Color', withT* `Color', alloca- `Color' peek*} -> `()' #}

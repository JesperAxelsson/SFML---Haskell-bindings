{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGAUGE TypeSynonymInstances #-}

module SFML.Graphics.Internal where

#include <SFML/Graphics.h>
#include "ColorWrapper.c"

import Foreign
import Foreign.C
import Foreign.Storable
import Data.Word

{#context lib="csfml-graphics" prefix="sf" #}


{------------------------------------  Color  ------------------------------------}

data Color = Color { red, green, blue, alpha :: Word8 } deriving (Eq, Show)

{#pointer *Color as ColorPtr -> Color #}

instance Storable Color where
  sizeOf _ = {#sizeof Color #}
  alignemnt _ = {#alignof Color #}
  peek ptr = do
    r <- fmap fromIntegral $ {#get Color->r#} ptr
    g <- fmap fromIntegral $ {#get Color->g#} ptr
    b <- fmap fromIntegral $ {#get Color->b#} ptr
    a <- fmap fromIntegral $ {#get Color->a#} ptr
    return (Color r g b a)
  poke ptr (Color r g b a) = do
    {#set Color->r} ptr (fromIntegral r)
    {#set Color->g} ptr (fromIntegral g)
    {#set Color->b} ptr (fromIntegral b)
    {#set Color->a} ptr (fromIntegral a)

black = Color 0 0 0 255
white = Color 255 255 255 255
red = Color 255 0 0 255
green = Color 0 255 0 255
blue = Color 0 0 255 255
yellow =  Color 255 255 0 255
magenta = Color 255 0 255 255
cyan = Color 0 255 255 255

{#fun unsafe Color_FromRGBWrapper as colorFromRGB
 {`Word8', `Word8', `Word8', alloca- `Color' peek*} -> `()'#}

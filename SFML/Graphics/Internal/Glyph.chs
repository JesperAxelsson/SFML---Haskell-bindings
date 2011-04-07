{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.Glyph where

#include <SFML/Graphics/Glyph.h>
#include "GlyphWrapper.c"

{#import SFML.Graphics.Internal.Types #}
{#import SFML.Graphics.Internal.Rect #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

{#context lib="csfml-graphics" prefix="sf" #}

instance Storable Glyph where
  sizeOf _ = {#sizeof Glyph #}
  alignment _ = {#alignof Glyph #}
  peek ptr = do
    advance <- fmap fromIntegral $ {#get Glyph.Advance#} ptr
    bounds <- alloca $ (\rectPtr -> do
      {#call unsafe Glyph_GetBounds #} ptr (castPtr rectPtr)
      peek rectPtr)
    subRect <- alloca $ (\rectPtr -> do
      {#call unsafe Glyph_GetSubRect #} ptr (castPtr rectPtr)
      peek rectPtr)
    return (Glyph advance bounds subRect)
  poke ptr (Glyph advance bounds subRect) = do
    {#set Glyph.Advance #} ptr (fromIntegral advance)
    withT bounds $ (\boundsPtr ->
                     {#call unsafe Glyph_SetBounds #} ptr (castPtr boundsPtr))
    withT subRect $ (\subRectPtr ->
                      {#call unsafe Glyph_SetSubRect #} ptr (castPtr subRectPtr))

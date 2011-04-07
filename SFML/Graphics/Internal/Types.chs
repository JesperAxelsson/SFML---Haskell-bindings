{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.Types where

#include <SFML/Graphics/Types.h>
#include <SFML/Graphics/BlendMode.h>
#include <SFML/Graphics/Color.h>
#include <SFML/Graphics/Glyph.h>

import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word

{#context lib="csfml-graphics" prefix="sf" #}

data Color = Color { red, green, blue, alpha :: Word8 } deriving (Eq, Show)

data Rect a = Rect { left, top, width, height :: a } deriving (Eq, Show)

data Glyph = Glyph { glyphAdvance :: Int
                   , glyphBounds, glyphSubRect :: Rect Int
                   } deriving (Eq, Show)

{#pointer *Font foreign newtype #}
{#pointer *Image foreign newtype #}
{#pointer *Shader foreign newtype #}
{#pointer *RenderImage foreign newtype #}
{#pointer *RenderWindow foreign newtype #}
{#pointer *Shape foreign newtype #}
{#pointer *Sprite foreign newtype #}
{#pointer *Text foreign newtype #}
{#pointer *View foreign newtype #}

{#pointer *Color as ColorPtr -> Color #}
{#pointer *Glyph as GlyphPtr -> Glyph #}

{#enum BlendMode {} deriving (Eq, Show) #}

{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.Types where

#include <SFML/Graphics/Types.h>
#include <SFML/Graphics/BlendMode.h>
#include <SFML/Graphics/Color.h>
#include <SFML/Graphics/Glyph.h>
#include <SFML/Graphics/Text.h>

{#import SFML.Window.Internal.Types #}
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.C.String
import Data.Word
import Data.IORef

{#context lib="csfml-graphics" prefix="sf" #}

data Color = Color { red, green, blue, alpha :: Word8 } deriving (Eq, Show)

data Rect a = Rect { left, top, width, height :: a } deriving (Eq, Show)

data Glyph = Glyph { glyphAdvance :: Int
                   , glyphBounds, glyphSubRect :: Rect Int
                   } deriving (Eq, Show)

{#pointer *Font as FontPtr foreign newtype #}
data Font = Font { fontPtr :: FontPtr
                 , fontData :: IORef (Maybe (ForeignPtr Word8))
                 }
withFont = withFontPtr . fontPtr

{#pointer *Image as ImagePtr foreign newtype #}
data Image = Image { imagePtr :: ImagePtr
                   }
withImage = withImagePtr . imagePtr

{#pointer *Shader foreign newtype #}

{#pointer *RenderImage foreign newtype #}

{#pointer *RenderWindow as RenderWindowPtr foreign newtype #}
data RenderWindow = RenderWindow { renderWindowPtr :: RenderWindowPtr
                                 , renderWindowTitle :: IORef (ForeignPtr CChar)
                                 , renderWindowContext :: IORef (ForeignPtr ContextSettings)
                                 , renderWindowIcon :: IORef (Maybe (ForeignPtr Word8))
                                 , renderWindowView :: IORef (Maybe View)
                                 }
withRenderWindow = withRenderWindowPtr . renderWindowPtr

{#pointer *Shape foreign newtype #}

{#pointer *Sprite as SpritePtr foreign newtype #}
data Sprite = Sprite { spritePtr :: SpritePtr
                     , spriteImage :: IORef (Maybe Image)
                     }
withSprite = withSpritePtr . spritePtr

{#pointer *Text as TextPtr foreign newtype #}
data Text = Text { textPtr :: TextPtr
                 , textString :: IORef (Maybe (ForeignPtr ()))
                 , textFont :: IORef (Maybe Font)}
withText = withTextPtr . textPtr

{#pointer *View foreign newtype #}

{#pointer *Color as ColorPtr -> Color #}

{#pointer *Glyph as GlyphPtr -> Glyph #}

{#enum BlendMode {} deriving (Eq, Show) #}

{#enum TextStyle {} deriving (Eq, Show) #}

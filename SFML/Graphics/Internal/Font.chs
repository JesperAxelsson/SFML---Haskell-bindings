{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.Font where

#include <SFML/Graphics/Font.h>
#include "FontWrapper.c"

{#import SFML.Graphics.Internal.Types #}
{#import SFML.Graphics.Internal.Glyph #}
{#import SFML.Graphics.Internal.Image #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Data.Char
import Data.Word
import Data.ByteString (ByteString)

{#context lib="csfml-graphics" prefix="sf" #}


foreign import ccall unsafe "&sfFont_Destroy"
  fontDestroy :: FinalizerPtr Font

mkFont :: Ptr Font -> IO Font
mkFont ptr = fmap Font $ newForeignPtr fontDestroy ptr

{#fun unsafe Font_CreateFromFile as ^
 {`String'} -> `Font' mkFont* #}

{#fun unsafe Font_CreateFromMemory as ^
 {withByteStringLen* `ByteString'&} -> `Font' mkFont* #}

{#fun unsafe Font_Copy as ^
 {withFont* `Font'} -> `Font' mkFont* #}

toCodepoint = fromIntegral.ord

{#fun unsafe Font_GetGlyphWrapper as fontGetGlyph
 {withFont* `Font'
 ,toCodepoint `Char'
 ,fromIntegral `Word'
 ,`Bool'
 ,alloca- `Glyph' peek*} -> `()' #}

{#fun unsafe Font_GetKerning as ^
 {withFont* `Font'
 ,toCodepoint `Char'
 ,toCodepoint `Char'
 ,fromIntegral `Word'} -> `Int' #}

{#fun unsafe Font_GetLineSpacing as ^
 {withFont* `Font'
 ,fromIntegral `Word'} -> `Int' #}

{#fun unsafe Font_GetImage as ^
 {withFont* `Font'
 ,fromIntegral `Word'} -> `Image' mkImage* #}

{#fun unsafe Font_GetDefaultFont as ^
 {} -> `Font' mkFont* #}

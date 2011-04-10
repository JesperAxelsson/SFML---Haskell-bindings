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
import qualified Data.ByteString.Internal as BSI
import Data.IORef

{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall unsafe "&sfFont_Destroy"
  fontDestroy :: FinalizerPtr FontPtr

mkFont :: Ptr FontPtr -> IO Font
mkFont ptr = do
  fontPtr <- fmap FontPtr $ newForeignPtr fontDestroy ptr
  fontData <- newIORef Nothing
  return (Font fontPtr fontData)

mkConstFont :: Ptr FontPtr -> IO Font
mkConstFont ptr = do
  fontPtr <- fmap FontPtr $ newForeignPtr_ ptr
  fontData <- newIORef Nothing
  return (Font fontPtr fontData)

{#fun unsafe Font_CreateFromFile as ^
 {`String'} -> `Maybe Font' 'fromNull mkFont'* #}

{#fun unsafe Font_CreateFromMemory as fontCreateFromMemory_
 {id `Ptr ()'
 ,id `CULong'} -> `Ptr FontPtr' id #}

fontCreateFromMemory :: ByteString -> IO (Maybe Font)
fontCreateFromMemory bytes = do
  let (dataFPtr, offset, len) = BSI.toForeignPtr bytes
  ptr <- withForeignPtr dataFPtr (\dataPtr -> fontCreateFromMemory_ (dataPtr `plusPtr` offset) (fromIntegral len))
  if ptr == nullPtr
    then return Nothing
    else do
    fontPtr <- fmap FontPtr $ newForeignPtr fontDestroy ptr
    fontData <- newIORef (Just dataFPtr)
    return (Just (Font fontPtr fontData))

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
 ,fromIntegral `Word'} -> `Image' mkConstImage* #}

{#fun unsafe Font_GetDefaultFont as ^
 {} -> `Font' mkConstFont* #}

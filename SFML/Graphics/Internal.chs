{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SFML.Graphics.Internal where

#include <SFML/Graphics.h>
#include "ColorWrapper.c"
#include "GlyphWrapper.c"
#include "FontWrapper.c"
#include "ImageWrapper.c"
#include "RenderWindowWrapper.c"

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Utils
import Data.Word
import SFML.ForeignUtils
import Data.Char
import Data.ByteString (ByteString, packCStringLen)

{#import SFML.Window.Internal #}

{#context lib="csfml-graphics" prefix="sf" #}


{------------------------------------  Color  ------------------------------------}

data Color = Color { red, green, blue, alpha :: Word8 } deriving (Eq, Show)

{#pointer *Color as ColorPtr -> Color #}

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


{-------------------------------------  Rect  --------------------------------------}

data Rect a = Rect { left, top, width, height :: a } deriving (Eq, Show)

instance Storable (Rect Float) where
  sizeOf _ = {#sizeof FloatRect #}
  alignment _ = {#alignof FloatRect #}
  peek ptr = do
    left <- fmap realToFrac $ {#get FloatRect.Left#} ptr
    top <- fmap realToFrac $ {#get FloatRect.Top#} ptr
    width <- fmap realToFrac $ {#get FloatRect.Width#} ptr
    height <- fmap realToFrac $ {#get FloatRect.Height#} ptr
    return (Rect left top width height)
  poke ptr (Rect left top width height) = do
    {#set FloatRect.Left #} ptr (realToFrac left)
    {#set FloatRect.Top #} ptr (realToFrac top)
    {#set FloatRect.Width #} ptr (realToFrac width)
    {#set FloatRect.Height #} ptr (realToFrac height)

instance Storable (Rect Int) where
  sizeOf _ = {#sizeof IntRect #}
  alignment _ = {#alignof IntRect #}
  peek ptr = do
    left <- fmap fromIntegral $ {#get IntRect.Left#} ptr
    top <- fmap fromIntegral $ {#get IntRect.Top#} ptr
    width <- fmap fromIntegral $ {#get IntRect.Width#} ptr
    height <- fmap fromIntegral $ {#get IntRect.Height#} ptr
    return (Rect left top width height)
  poke ptr (Rect left top width height) = do
    {#set IntRect.Left #} ptr (fromIntegral left)
    {#set IntRect.Top #} ptr (fromIntegral top)
    {#set IntRect.Width #} ptr (fromIntegral width)
    {#set IntRect.Height #} ptr (fromIntegral height)

class Rectable a where
  rectContains :: Rect a -> a -> a -> Bool
  rectIntersection :: Rect a -> Rect a -> Maybe (Rect a)

instance Rectable Float where
  rectContains rect x y = unsafePerformIO $
                          withT rect $ \rectPtr ->
                          {#call unsafe FloatRect_Contains#} (castPtr rectPtr) (realToFrac x) (realToFrac y) >>= \b ->
                          return (toBool b)
  rectIntersection rect1 rect2 = unsafePerformIO $
                                 withT rect1 $ \rect1Ptr ->
                                 withT rect2 $ \rect2Ptr ->
                                 alloca $ \outRectPtr -> do
                                 intersects <- {#call unsafe FloatRect_Intersects#} (castPtr rect1Ptr) (castPtr rect2Ptr) (castPtr outRectPtr)
                                 if toBool intersects
                                   then fmap Just (peek outRectPtr)
                                   else return Nothing

instance Rectable Int where
  rectContains rect x y = unsafePerformIO $
                          withT rect $ \rectPtr ->
                          {#call unsafe IntRect_Contains#} (castPtr rectPtr) (fromIntegral x) (fromIntegral y) >>= \b ->
                          return (toBool b)
  rectIntersection rect1 rect2 = unsafePerformIO $
                                 withT rect1 $ \rect1Ptr ->
                                 withT rect2 $ \rect2Ptr ->
                                 alloca $ \outRectPtr -> do
                                 intersects <- {#call unsafe IntRect_Intersects#} (castPtr rect1Ptr) (castPtr rect2Ptr) (castPtr outRectPtr)
                                 if toBool intersects
                                   then fmap Just (peek outRectPtr)
                                   else return Nothing

peekRect :: Storable (Rect a) => Ptr () -> IO (Rect a)
peekRect ptr = peek (castPtr ptr)

allocaIntRect :: (Ptr () -> IO b) -> IO b
allocaIntRect a = alloca $ \(ptr :: Ptr (Rect Int)) -> a (castPtr ptr)

allocaFloatRect :: (Ptr () -> IO b) -> IO b
allocaFloatRect a = alloca $ \(ptr :: Ptr (Rect Float)) -> a (castPtr ptr)

{-------------------------------------  Font  --------------------------------------}

data Glyph = Glyph { glyphAdvance :: Int
                   , glyphBounds, glyphSubRect :: Rect Int
                   } deriving (Eq, Show)

{#pointer *Glyph as GlyphPtr -> Glyph #}

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

{#pointer *Font foreign newtype #}

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

{#fun unsafe Font_GetDefaultFont as ^
 {} -> `Font' mkFont* #}


{------------------------------------  Sprite  ------------------------------------}

{#pointer *Sprite foreign newtype #}

{------------------------------------  Shape  ------------------------------------}

{#pointer *Shape foreign newtype #}

{-------------------------------------  Text  --------------------------------------}

{#pointer *Text foreign newtype #}

{------------------------------------  Shader  ------------------------------------}

{#pointer *Shader foreign newtype #}


{-------------------------------------  View  --------------------------------------}

{#pointer *View foreign newtype #}

foreign import ccall unsafe "&sfView_Destroy"
  viewDestroy :: FinalizerPtr View

mkView :: Ptr View -> IO View
mkView ptr = fmap View $ newForeignPtr viewDestroy ptr

mkConstView :: Ptr View -> IO View
mkConstView ptr = fmap View $ newForeignPtr_ ptr

{---------------------------------  RenderWindow  ----------------------------------}

{#pointer *RenderWindow foreign newtype #}

foreign import ccall unsafe "&sfRenderWindow_Destroy"
  renderWindowDestroy :: FinalizerPtr RenderWindow

mkRenderWindow :: Ptr RenderWindow -> IO RenderWindow
mkRenderWindow ptr = fmap RenderWindow $ newForeignPtr renderWindowDestroy ptr

{#fun unsafe RenderWindow_CreateWrapper as renderWindowCreate
 {withT* `VideoMode'
 ,`String'
 ,stylesToCULong `[Style]'
 ,withT* `ContextSettings'} -> `RenderWindow' mkRenderWindow* #}

{#fun unsafe RenderWindow_CreateFromHandle as ^
 {id `WindowHandle'
 ,withT* `ContextSettings'} -> `RenderWindow' mkRenderWindow* #}

{#fun unsafe RenderWindow_Close as ^
 {withRenderWindow* `RenderWindow'} -> `()' #}

{#fun unsafe RenderWindow_IsOpened as ^
 {withRenderWindow* `RenderWindow'} -> `Bool' #}

{#fun unsafe RenderWindow_GetWidth as ^
 {withRenderWindow* `RenderWindow'} -> `Word' fromIntegral #}

{#fun unsafe RenderWindow_GetHeight as ^
 {withRenderWindow* `RenderWindow'} -> `Word' fromIntegral #}

{#fun unsafe RenderWindow_GetSettingsWrapper as renderWindowGetSettings
 {withRenderWindow* `RenderWindow'
 ,alloca- `ContextSettings' peek*} -> `()' #}

renderWindowGetEvent :: RenderWindow -> IO (Maybe Event)
renderWindowGetEvent window =
  withRenderWindow window $ \winPtr ->
  alloca $ \evtPtr ->
  {#call unsafe RenderWindow_GetEvent#} winPtr evtPtr >>= \b ->
  if toBool b then do
    evt <- peek evtPtr
    return (Just evt)
  else return Nothing

renderWindowWaitEvent :: RenderWindow -> IO (Maybe Event)
renderWindowWaitEvent window =
  withRenderWindow window $ \winPtr ->
  alloca $ \evtPtr ->
  {#call unsafe RenderWindow_WaitEvent#} winPtr evtPtr >>= \b ->
  if toBool b then do
    evt <- peek evtPtr
    return (Just evt)
  else return Nothing
       
{#fun unsafe RenderWindow_EnableVerticalSync as ^
 {withRenderWindow* `RenderWindow'
 ,`Bool'} -> `()' #}

{#fun unsafe RenderWindow_ShowMouseCursor as ^
 {withRenderWindow* `RenderWindow'
 ,`Bool'} -> `()' #}

{#fun unsafe RenderWindow_SetCursorPosition as ^
 {withRenderWindow* `RenderWindow'
 ,fromIntegral `Word'
 ,fromIntegral `Word'} -> `()' #}

{#fun unsafe RenderWindow_SetPosition as ^
 {withRenderWindow* `RenderWindow'
 ,`Int'
 ,`Int'} -> `()' #}

{#fun unsafe RenderWindow_SetSize as ^
 {withRenderWindow* `RenderWindow'
 ,fromIntegral `Word'
 ,fromIntegral `Word'} -> `()' #}

{#fun unsafe RenderWindow_Show as ^
 {withRenderWindow* `RenderWindow'
 ,`Bool'} -> `()' #}

{#fun unsafe RenderWindow_EnableKeyRepeat as ^
 {withRenderWindow* `RenderWindow'
 ,`Bool'} -> `()' #}

{#fun unsafe RenderWindow_SetIcon as ^
 {withRenderWindow* `RenderWindow'
 ,fromIntegral `Word'
 ,fromIntegral `Word'
 ,withByteString* `ByteString'} -> `()' #}

{#fun unsafe RenderWindow_SetActive as ^
 {withRenderWindow* `RenderWindow'
 ,`Bool'} -> `Bool' #}

{#fun unsafe RenderWindow_SaveGLStates as ^
 {withRenderWindow* `RenderWindow'} -> `()' #}

{#fun unsafe RenderWindow_RestoreGLStates as ^
 {withRenderWindow* `RenderWindow'} -> `()' #}

{#fun unsafe RenderWindow_Display as ^
 {withRenderWindow* `RenderWindow'} -> `()' #}

{#fun unsafe RenderWindow_GetInput as ^
 {withRenderWindow* `RenderWindow'} -> `Input' id #}

{#fun unsafe RenderWindow_SetFramerateLimit as ^
 {withRenderWindow* `RenderWindow'
 ,fromIntegral `Word'} -> `()' #}

{#fun unsafe RenderWindow_GetFrameTime as ^
 {withRenderWindow* `RenderWindow'} -> `Float' #}

{#fun unsafe RenderWindow_SetJoystickThreshold as ^
 {withRenderWindow* `RenderWindow'
 ,`Float'} -> `()' #}

{#fun unsafe RenderWindow_GetSystemHandle as ^
 {withRenderWindow* `RenderWindow'} -> `WindowHandle' id #}

{#fun unsafe RenderWindow_DrawSprite as ^
 {withRenderWindow* `RenderWindow'
 ,withSprite* `Sprite'} -> `()' #}

{#fun unsafe RenderWindow_DrawShape as ^
 {withRenderWindow* `RenderWindow'
 ,withShape* `Shape'} -> `()' #}

{#fun unsafe RenderWindow_DrawText as ^
 {withRenderWindow* `RenderWindow'
 ,withText* `Text'} -> `()' #}

{#fun unsafe RenderWindow_DrawSpriteWithShader as ^
 {withRenderWindow* `RenderWindow'
 ,withSprite* `Sprite'
 ,withShader* `Shader'} -> `()' #}

{#fun unsafe RenderWindow_DrawShapeWithShader as ^
 {withRenderWindow* `RenderWindow'
 ,withShape* `Shape'
 ,withShader* `Shader'} -> `()' #}

{#fun unsafe RenderWindow_DrawTextWithShader as ^
 {withRenderWindow* `RenderWindow'
 ,withText* `Text'
 ,withShader* `Shader'} -> `()' #}

class Drawable a where
  draw :: RenderWindow -> a -> IO ()
  drawWithShader :: RenderWindow -> a -> Shader -> IO ()

instance Drawable Sprite where
  draw = renderWindowDrawSprite
  drawWithShader = renderWindowDrawSpriteWithShader

instance Drawable Shape where
  draw = renderWindowDrawShape
  drawWithShader = renderWindowDrawShapeWithShader

instance Drawable Text where
  draw = renderWindowDrawText
  drawWithShader = renderWindowDrawTextWithShader

{#fun unsafe RenderWindow_ClearWrapper as renderWindowClear
 {withRenderWindow* `RenderWindow'
 ,withT* `Color'} -> `()' #}

{#fun unsafe RenderWindow_GetView as ^
 {withRenderWindow* `RenderWindow'} -> `View' mkConstView* #}

{#fun unsafe RenderWindow_GetDefaultView as ^
 {withRenderWindow* `RenderWindow'} -> `View' mkConstView* #}

{#fun unsafe RenderWindow_GetViewportWrapper as renderWindowGetViewport
 {withRenderWindow* `RenderWindow'
 ,withView* `View'
 ,allocaIntRect- `Rect Int' peekRect*} -> `()' #}

{#fun unsafe RenderWindow_ConvertCoords as renderWindowConvertCoords_
 {withRenderWindow* `RenderWindow'
 ,fromIntegral `Word'
 ,fromIntegral `Word'
 ,alloca- `CFloat' peek*
 ,alloca- `CFloat' peek*
 ,withView* `View'} -> `()'#}

renderWindowConvertCoords :: RenderWindow -> Word -> Word -> View -> IO (Float, Float)
renderWindowConvertCoords win x y view = do
  (vx, vy) <- renderWindowConvertCoords_ win x y view
  return (realToFrac vx, realToFrac vy)

{------------------------------------  Image  ------------------------------------}

{#pointer *Image foreign newtype #}

{#fun unsafe Font_GetImage as ^
 {withFont* `Font'
 ,fromIntegral `Word'} -> `Image' mkImage* #}

foreign import ccall unsafe "&sfImage_Destroy"
  imageDestroy :: FinalizerPtr Image

mkImage :: Ptr Image -> IO Image
mkImage ptr = fmap Image $ newForeignPtr imageDestroy ptr

{#fun unsafe Image_CreateFromColorWrapper as imageCreateFromColor
 {fromIntegral `Word'
 ,fromIntegral `Word'
 ,withT* `Color'} -> `Image' mkImage* #}

{#fun unsafe Image_CreateFromPixels as ^
 {fromIntegral `Word'
 ,fromIntegral `Word'
 ,withByteString* `ByteString'} -> `Image' mkImage* #}

{#fun unsafe Image_CreateFromFile as ^
 {`String'} -> `Image' mkImage* #}

{#fun unsafe Image_CreateFromMemory as ^
 {withByteStringLen* `ByteString'&} -> `Image' mkImage* #}

{#fun unsafe Image_Copy as ^
 {withImage* `Image'} -> `Image' mkImage* #}

{#fun unsafe Image_SaveToFile as ^
 {withImage* `Image'
 ,`String'} -> `()' #}

{#fun unsafe Image_CreateMaskFromColorWrapper as imageCreateMaskFromColor
 {withImage* `Image'
 ,withT* `Color'
 ,`Word8'} -> `()' #}

{#fun unsafe Image_CopyImageWrapper as imageCopyImage
 {withImage* `Image'
 ,withImage* `Image'
 ,fromIntegral `Word'
 ,fromIntegral `Word'
 ,castWithT* `Rect Int'} -> `()' #}

{#fun unsafe Image_CopyScreenWrapper as imageCopyScreen
 {withImage* `Image'
 ,withRenderWindow* `RenderWindow'
 ,castWithT* `Rect Int'} -> `Bool' #}

{#fun unsafe Image_SetPixelWrapper as imageSetPixel
 {withImage* `Image'
 ,fromIntegral `Word'
 ,fromIntegral `Word'
 ,withT* `Color'} -> `()' #}

{#fun unsafe Image_GetPixelWrapper as imageGetPixel
 {withImage* `Image'
 ,fromIntegral `Word'
 ,fromIntegral `Word'
 ,alloca- `Color' peek*} -> `()' #}

{#fun unsafe Image_GetPixelsPtr as imageGetPixelsPtr
 {withImage* `Image'} -> `Ptr CChar' castPtr #}

imageGetPixels :: Image -> IO ByteString
imageGetPixels img = do
  width <- imageGetWidth img
  height <- imageGetHeight img
  let len = fromIntegral $ width * height * 4
  ptr <- imageGetPixelsPtr img
  packCStringLen (ptr, len)
  
{#fun unsafe Image_UpdatePixelsWrapper as imageUpdatePixels
 {withImage* `Image'
 ,withByteString* `ByteString'
 ,castWithT* `Rect Int'} -> `()' #}

{#fun unsafe Image_Bind as ^
 {withImage* `Image'} -> `()' #}

{#fun unsafe Image_SetSmooth as ^
 {withImage* `Image', `Bool'} -> `()' #}

{#fun unsafe Image_GetWidth as ^
 {withImage* `Image'} -> `Word' fromIntegral #}

{#fun unsafe Image_GetHeight as ^
 {withImage* `Image'} -> `Word' fromIntegral #}

{#fun unsafe Image_IsSmooth as ^
 {withImage* `Image'} -> `Bool' #}


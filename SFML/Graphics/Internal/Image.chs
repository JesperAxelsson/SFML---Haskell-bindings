{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.Image where

#include <SFML/Graphics/Image.h>
#include "ImageWrapper.c"

{#import SFML.Graphics.Internal.Types #}
{#import SFML.Graphics.Internal.Color #}
{#import SFML.Graphics.Internal.Rect #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Word
import Data.ByteString (ByteString, packCStringLen)

{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall "&sfImage_Destroy"
  imageDestroy :: FinalizerPtr Image

mkImage :: Ptr Image -> IO Image
mkImage ptr = fmap Image $ newForeignPtr imageDestroy ptr

mkConstImage :: Ptr Image -> IO Image
mkConstImage ptr = fmap Image $ newForeignPtr_ ptr

{#fun unsafe Image_CreateFromColorWrapper as imageCreateFromColor
 {fromIntegral `Word'
 ,fromIntegral `Word'
 ,withT* `Color'} -> `Maybe Image' 'fromNull mkImage'* #}

{#fun unsafe Image_CreateFromPixels as ^
 {fromIntegral `Word'
 ,fromIntegral `Word'
 ,'withByteString (undefined :: CUChar)'* `ByteString'} -> `Maybe Image' 'fromNull mkImage'* #}

{#fun unsafe Image_CreateFromFile as ^
 {`String'} -> `Maybe Image' 'fromNull mkImage'* #}

{#fun unsafe Image_CreateFromMemory as ^
 {'withByteStringLen ()'* `ByteString'&} -> `Maybe Image' 'fromNull mkImage'* #}

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
 ,'withByteString (undefined :: CUChar)'* `ByteString'
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

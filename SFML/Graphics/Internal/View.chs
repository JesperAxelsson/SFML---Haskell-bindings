{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.View where

#include <SFML/Graphics/View.h>

{#import SFML.Graphics.Internal.Types #}
import SFML.ForeignUtils
import Foreign.Ptr
import Foreign.ForeignPtr

{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall unsafe "&sfView_Destroy"
  viewDestroy :: FinalizerPtr View

mkView :: Ptr View -> IO View
mkView ptr = fmap View $ newForeignPtr viewDestroy ptr

mkConstView :: Ptr View -> IO View
mkConstView ptr = fmap View $ newForeignPtr_ ptr

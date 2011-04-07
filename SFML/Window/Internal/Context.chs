{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Window.Internal.Context where

#include <SFML/Window/Context.h>

{#import SFML.Window.Internal.Types #}
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Data.Word

{#context lib="csfml-window" prefix="sf" #}

foreign import ccall unsafe "&sfContext_Destroy"
  contextDestroy :: FinalizerPtr Context

mkContext :: Ptr Context -> IO Context
mkContext ctx = do
  ctx' <- newForeignPtr contextDestroy ctx
  return (Context ctx')

{#fun unsafe Context_Create as ^
 {} -> `Context' mkContext* #}

{#fun unsafe Context_SetActive as ^
 {withContext* `Context', `Bool'} -> `()' #}

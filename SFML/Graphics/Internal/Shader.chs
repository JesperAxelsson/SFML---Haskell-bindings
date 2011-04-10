{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Graphics.Internal.Shader where

#include <SFML/Graphics/Shader.h>

{#import SFML.Graphics.Internal.Types #}
{#import SFML.Graphics.Internal.Image #}
import SFML.ForeignUtils
import Foreign.C
import Foreign.Storable
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.ByteString (ByteString)

{#context lib="csfml-graphics" prefix="sf" #}

foreign import ccall unsafe "&sfShader_Destroy"
  shaderDestroy :: FinalizerPtr Shader

mkShader :: Ptr Shader -> IO Shader
mkShader ptr = fmap Shader $ newForeignPtr shaderDestroy ptr

{#fun unsafe Shader_CreateFromFile as ^
 {`String'} -> `Maybe Shader' 'fromNull mkShader'* #}

{#fun unsafe Shader_CreateFromMemory as ^
 {'withByteString (undefined :: CChar)'* `ByteString'} -> `Maybe Shader' 'fromNull mkShader'* #}

{#fun unsafe Shader_Copy as ^
 {withShader* `Shader'} -> `Shader' mkShader* #}

{#fun unsafe Shader_SetParameter1 as ^
 {withShader* `Shader'
 ,`String'
 ,`Float'} -> `()' #}

{#fun unsafe Shader_SetParameter2 as ^
 {withShader* `Shader'
 ,`String'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Shader_SetParameter3 as ^
 {withShader* `Shader'
 ,`String'
 ,`Float'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Shader_SetParameter4 as ^
 {withShader* `Shader'
 ,`String'
 ,`Float'
 ,`Float'
 ,`Float'
 ,`Float'} -> `()' #}

{#fun unsafe Shader_SetTexture as ^
 {withShader* `Shader'
 ,`String'
 ,withImage* `Image'} -> `()' #}

{#fun unsafe Shader_SetCurrentTexture as ^
 {withShader* `Shader'
 ,`String'} -> `()' #}

{#fun unsafe Shader_Bind as ^
 {withShader* `Shader'} -> `()' #}

{#fun unsafe Shader_Unbind as ^
 {withShader* `Shader'} -> `()' #}

{#fun pure unsafe Shader_IsAvailable as ^
 {} -> `Bool' #}


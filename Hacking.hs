import SFML.Graphics.Internal.RenderWindow
import SFML.Graphics.Internal.Image
import SFML.Graphics.Internal.Sprite
import SFML.Graphics.Internal.Color
import SFML.Graphics.Internal.Text
import SFML.Graphics.Internal.Font
import SFML.Window.Internal.Types
import SFML.Window.Internal.VideoMode
import SFML.Window.Internal.Event
import SFML.System.Internal.Clock
import SFML.Audio.Internal.Music
import Control.Monad

import Foreign.ForeignPtr
import SFML.Graphics.Internal.Types

withMaybe :: Monad m => Maybe a -> (a -> m b) -> m ()
withMaybe Nothing _ = return ()
withMaybe (Just x) m = m x >> return ()

main = do
  window <- renderWindowCreate (VideoMode 800 600 32) "SFML window" [] Nothing
  renderWindowEnableVerticalSync window True
  renderWindowSetFramerateLimit window 5
  Just image <- imageCreateFromFile "test.png"
  Just sprite <- spriteCreate
  spriteSetImage sprite image True
  Just text <- textCreate
  textSetFont text fontDefault
  textSetString text "Hello"
  textSetCharacterSize text 50
  
  Just music <- musicCreateFromFile "Music.ogg"
  musicPlay music
  
  run window sprite text music
  putStrLn "Main exiting"
  
run window sprite text music = do
  isOpen <- renderWindowIsOpened window
  when isOpen $ do
    handleEvents window sprite music
    renderWindowClear window colorBlack
    renderWindowDrawSprite window sprite
    renderWindowDrawText window text
    renderWindowDisplay window
    run window sprite text music
  
 where
   handleEvents window sprite music = do
     event <- renderWindowPollEvent window
     withMaybe event $ \evt ->do
       case evt of
         Closed -> renderWindowClose window
         (KeyPressed KeyEscape _ _ _) -> renderWindowClose window
         (KeyPressed KeyLeft _ _ _) -> spriteMove sprite (-5) 0
         (KeyPressed KeyRight _ _ _) -> spriteMove sprite 5 0
         (KeyPressed KeyUp _ _ _) -> spriteMove sprite 0 (-5)
         (KeyPressed KeyDown _ _ _) -> spriteMove sprite 0 5
         (KeyPressed KeyP _ _ _) -> musicPlay music
         (KeyPressed KeyS _ _ _) -> musicStop music
         (KeyPressed key _ _ _) -> putStrLn ("Pressed " ++ show key)
         _ -> return ()
       handleEvents window sprite music


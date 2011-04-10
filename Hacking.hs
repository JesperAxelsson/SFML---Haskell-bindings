import SFML.Graphics.Internal.RenderWindow
import SFML.Graphics.Internal.Image
import SFML.Graphics.Internal.Sprite
import SFML.Graphics.Internal.Color
import SFML.Window.Internal.Types
import SFML.Window.Internal.VideoMode
import SFML.Window.Internal.Event
import SFML.System.Internal.Clock
import Control.Monad

withMaybe :: Monad m => Maybe a -> (a -> m b) -> m ()
withMaybe Nothing _ = return ()
withMaybe (Just x) m = m x >> return ()

main = do
  window <- renderWindowCreate (VideoMode 800 600 32) "SFML window" [] Nothing
  Just image <- imageCreateFromFile "test.png"
  Just sprite <- spriteCreate
  clock <- clockCreate
  spriteSetImage sprite image True
  run window sprite
  imageGetWidth image
  
  
run window sprite = do
  isOpen <- renderWindowIsOpened window
  when isOpen $ do
    handleEvents window sprite
    renderWindowClear window colorBlack
    renderWindowDrawSprite window sprite
    renderWindowDisplay window
    run window sprite
  
 where
   handleEvents window sprite = do
     event <- renderWindowGetEvent window
     withMaybe event $ \evt ->do
       case evt of
         Closed -> renderWindowClose window
         (KeyPressed KeyEscape _ _ _) -> renderWindowClose window
         (KeyPressed KeyLeft _ _ _) -> spriteMove sprite (-5) 0
         (KeyPressed KeyRight _ _ _) -> spriteMove sprite 5 0
         (KeyPressed KeyUp _ _ _) -> spriteMove sprite 0 (-5)
         (KeyPressed KeyDown _ _ _) -> spriteMove sprite 0 5
         (KeyPressed key _ _ _) -> putStrLn ("Pressed " ++ show key)
         _ -> return ()
       handleEvents window sprite

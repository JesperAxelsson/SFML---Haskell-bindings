import SFML.System.Internal
import SFML.Window.Internal

main = do
  w <- windowCreateSimple (VideoMode 800 600 32) "Hello world" [DefaultStyle]
  go w
 where
   go w = do
     exit <- processEvents w
     if not exit
       then do
       windowDisplay w
       go w
       else
       return ()
   processEvents w = do
     mEvt <- windowGetEvent w
     case mEvt of
       Nothing -> return False
       Just (Closed) -> do
         windowClose w
         return True
       Just _ -> return False

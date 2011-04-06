import SFML.System.Internal
import Control.Concurrent

main = do
  time <- clockGetTime c
  c <- clockCreate
  print time
  threadDelay 1000000
  time <- clockGetTime c
  print time

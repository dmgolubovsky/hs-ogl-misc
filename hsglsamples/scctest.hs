-- SCC test.

import Control.Concurrent.Configuration
import Control.Monad.Coroutine
import Control.Concurrent.SCC.Streams
import Control.Concurrent.SCC.Types
import Control.Concurrent.SCC.Components hiding ((&&), (||))


main :: IO ()

main = do
  runCoroutine (pipe (produce $ with fromStdIn)
                     (consume $ with toStdOut))
  return ()


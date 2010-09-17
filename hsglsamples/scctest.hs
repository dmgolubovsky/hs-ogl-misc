-- SCC test.

import System.IO
import Control.Monad
import Control.Monad.Trans

import Control.Monad.Coroutine
import Control.Concurrent.SCC.Types
import Control.Concurrent.SCC.Streams
import Control.Concurrent.SCC.Primitives


main :: IO ()

main = do
  putStrLn "Testing SCC"
  runXIO $ runCoroutine (produce fromStdIn' `pipe` consume toStdOut')
  return ()

data XIO m a =
   Return a
 | Lift (m a)
 | Error String

runXIO :: (Monad m) => XIO m a -> m a
runXIO (Lift x) = x
runXIO (Return x) = return x
runXIO (Error s) = fail s

instance (Monad m) => Monad (XIO m) where
  Lift x >>= f = Lift (x >>= runXIO . f)
  Return x >>= f = Lift (return x >>= runXIO . f)
  Error s >>= _ = Error s
  return = Return
  fail = Error

pc = Lift . putChar
gc = Lift getChar
eof = Lift isEOF


toStdOut' :: Consumer (XIO IO) Char ()
toStdOut' = Consumer (mapMStream_ (\x-> lift (pc x)))


fromStdIn' :: Producer (XIO IO) Char ()
fromStdIn' = Producer (unmapMStream_ (lift eof >>= 
                                     cond (return Nothing) (lift (liftM Just gc))))


instance (Monad m) => Functor (XIO m) where
  fmap f x = x >>= Return . f 

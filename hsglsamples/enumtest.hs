module Main (main) where

-- Initially based on John Millikin's line counting program.

import Data.Enumerator
import Data.Enumerator.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import System.IO
import System.Environment

iterLines :: Monad m => Iteratee B.ByteString m Integer
iterLines = continue (step 0) where
        step acc EOF = yield acc EOF
        step acc (Chunks xs) = continue $ step $! foldl foldStep acc xs
        foldStep acc bytes = acc + countChar '\n' bytes

countChar :: Char -> B.ByteString -> Integer
countChar c = B8.foldl (\acc c' -> if c' == c then acc + 1 else acc) 0

main :: IO ()
main = run (enumHandle 409 stdin $$ 
            joinI $ Data.Enumerator.map B.reverse $$ 
            joinI $ Data.Enumerator.map B.reverse $$ 
            iterHandle stdout) >>= print


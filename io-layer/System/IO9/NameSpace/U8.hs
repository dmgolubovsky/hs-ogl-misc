{-# LANGUAGE CPP, MagicHash #-}

-- |
-- Module      : System.IO9.NameSpace.U8 (stuff gone/underexposed in the new text package)
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- Basic UTF-8 validation and character manipulation.
module System.IO9.NameSpace.U8
    (
    -- Decomposition
      ord2
    , ord3
    , ord4
    -- Construction
    , chr2
    , chr3
    , chr4
    -- Validation
    , validate1
    , validate2
    , validate3
    , validate4
    -- Other
    , spanBy
    , decodeUtf8Part
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Bits ((.&.))
import GHC.Exts
import GHC.Base hiding (ord, unsafeChr)
import GHC.Word
import Control.Monad.ST (ST)
import Data.Text.Internal
import qualified Data.Text.Array as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

default(Int)

between :: Word8                -- ^ byte to check
        -> Word8                -- ^ lower bound
        -> Word8                -- ^ upper bound
        -> Bool
between x y z = x >= y && x <= z
{-# INLINE between #-}

ord2 :: Char -> (Word8,Word8)
ord2 c =
#if defined(ASSERTS)
    assert (n >= 0x80 && n <= 0x07ff)
#endif
    (x1,x2)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 6) + 0xC0
      x2 = fromIntegral $ (n .&. 0x3F)   + 0x80

ord3 :: Char -> (Word8,Word8,Word8)
ord3 c =
#if defined(ASSERTS)
    assert (n >= 0x0800 && n <= 0xffff)
#endif
    (x1,x2,x3)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 12) + 0xE0
      x2 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x3 = fromIntegral $ (n .&. 0x3F) + 0x80

ord4 :: Char -> (Word8,Word8,Word8,Word8)
ord4 c =
#if defined(ASSERTS)
    assert (n >= 0x10000)
#endif
    (x1,x2,x3,x4)
    where
      n  = ord c
      x1 = fromIntegral $ (n `shiftR` 18) + 0xF0
      x2 = fromIntegral $ ((n `shiftR` 12) .&. 0x3F) + 0x80
      x3 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x4 = fromIntegral $ (n .&. 0x3F) + 0x80

chr2_16 :: Word16 -> Word16 -> Char
chr2_16 (W16# a#) (W16# b#) = C# (chr# (upper# +# lower# +# 0x10000#))
    where
      !x# = word2Int# a#
      !y# = word2Int# b#
      !upper# = uncheckedIShiftL# (x# -# 0xD800#) 10#
      !lower# = y# -# 0xDC00#
{-# INLINE chr2_16 #-}

chr2 :: Word8 -> Word8 -> Char
chr2 (W8# x1#) (W8# x2#) = C# (chr# (z1# +# z2#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
      !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3 :: Word8 -> Word8 -> Word8 -> Char
chr3 (W8# x1#) (W8# x2#) (W8# x3#) = C# (chr# (z1# +# z2# +# z3#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
      !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4             :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    C# (chr# (z1# +# z2# +# z3# +# z4#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !y4# = word2Int# x4#
      !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
      !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
      !z4# = y4# -# 0x80#
{-# INLINE chr4 #-}

validate1 :: Word8 -> Bool
validate1 x1 = x1 <= 0x7F
{-# INLINE validate1 #-}

validate2 :: Word8 -> Word8 -> Bool
validate2 x1 x2 = between x1 0xC2 0xDF && between x2 0x80 0xBF
{-# INLINE validate2 #-}

validate3 :: Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate3 #-}
validate3 x1 x2 x3 = validate3_1 || validate3_2 || validate3_3 || validate3_4
  where
    validate3_1 = (x1 == 0xE0) &&
                  between x2 0xA0 0xBF &&
                  between x3 0x80 0xBF
    validate3_2 = between x1 0xE1 0xEC &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF
    validate3_3 = x1 == 0xED &&
                  between x2 0x80 0x9F &&
                  between x3 0x80 0xBF
    validate3_4 = between x1 0xEE 0xEF &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF

validate4 :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate4 #-}
validate4 x1 x2 x3 x4 = validate4_1 || validate4_2 || validate4_3
  where 
    validate4_1 = x1 == 0xF0 &&
                  between x2 0x90 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_2 = between x1 0xF1 0xF3 &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_3 = x1 == 0xF4 &&
                  between x2 0x80 0x8F &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF


class UnsafeShift a where
    shiftL :: a -> Int -> a
    shiftR :: a -> Int -> a

instance UnsafeShift Word16 where
    {-# INLINE shiftL #-}
    shiftL (W16# x#) (I# i#) = W16# (narrow16Word# (x# `uncheckedShiftL#` i#))

    {-# INLINE shiftR #-}
    shiftR (W16# x#) (I# i#) = W16# (x# `uncheckedShiftRL#` i#)

instance UnsafeShift Word32 where
    {-# INLINE shiftL #-}
    shiftL (W32# x#) (I# i#) = W32# (narrow32Word# (x# `uncheckedShiftL#` i#))

    {-# INLINE shiftR #-}
    shiftR (W32# x#) (I# i#) = W32# (x# `uncheckedShiftRL#` i#)

instance UnsafeShift Word64 where
    {-# INLINE shiftL #-}
    shiftL (W64# x#) (I# i#) = W64# (x# `uncheckedShiftL64#` i#)

    {-# INLINE shiftR #-}
    shiftR (W64# x#) (I# i#) = W64# (x# `uncheckedShiftRL64#` i#)

instance UnsafeShift Int where
    {-# INLINE shiftL #-}
    shiftL (I# x#) (I# i#) = I# (x# `iShiftL#` i#)

    {-# INLINE shiftR #-}
    shiftR (I# x#) (I# i#) = I# (x# `iShiftRA#` i#)

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
{-# INLINE ord #-}

-- Decode a 'ByteString' possibly partially, returning the succesfully
-- decoded portion of 'Text' along with the remainder of the 'ByteString'
-- which could not be decoded.
 

decodeUtf8Part :: B.ByteString -> (Text, B.ByteString)
decodeUtf8Part bs = (textP (fst a) 0 (fst (snd a)), (snd (snd a)))
 where
  a   = A.run2 (A.new len >>= outer 0 0)
  len = B.length bs
  outer n0 m0 arr = go n0 m0
   where
    go !n !m = do
      let x1 = idx m
          x2 = idx (m + 1)
          x3 = idx (m + 2)
          x4 = idx (m + 3)
          idx = B.unsafeIndex bs
      case undefined of
       _| m >= len -> return (arr, (n, B.empty))
        | validate1 x1 -> do
           A.unsafeWrite arr n (fromIntegral x1)
           go (n+1) (m+1)
        | m+1 < len && validate2 x1 x2 -> do
           w <- unsafeWrite arr n (chr2 x1 x2)
           go (n+w) (m+2)
        | m+2 < len && validate3 x1 x2 x3 -> do
           w <- unsafeWrite arr n (chr3 x1 x2 x3)
           go (n+w) (m+3)
        | m+3 < len && validate4 x1 x2 x3 x4 -> do
           w <- unsafeWrite arr n (chr4 x1 x2 x3 x4)
           go (n+w) (m+4)
        | otherwise -> return (arr, (n, B.drop m bs))


spanBy :: (Char -> Bool) -> Text -> (Text, Text)
spanBy p t@(Text arr off len) = (textP arr off k, textP arr (off+k) (len-k))
  where k = loop 0
        loop !i | i >= len || not (p c) = i
                | otherwise             = loop (i+d)
            where Iter c d              = iter t i

{-# INLINE spanBy #-}

data Iter = Iter {-# UNPACK #-} !Char {-# UNPACK #-} !Int

iter :: Text -> Int -> Iter
iter (Text arr off _len) i
    | m < 0xD800 || m > 0xDBFF = Iter (unsafeChr m) 1
    | otherwise                = Iter (chr2_16 m n) 2
  where m = A.unsafeIndex arr j
        n = A.unsafeIndex arr k
        j = off + i
        k = j + 1
{-# INLINE iter #-}

unsafeChr :: Word16 -> Char
unsafeChr (W16# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr #-}

unsafeWrite :: A.MArray s -> Int -> Char -> ST s Int
unsafeWrite marr i c
    | n < 0x10000 = do
#if defined(ASSERTS)
        assert (i >= 0) . assert (i < A.length marr) $ return ()
#endif
        A.unsafeWrite marr i (fromIntegral n)
        return 1
    | otherwise = do
#if defined(ASSERTS)
        assert (i >= 0) . assert (i < A.length marr - 1) $ return ()
#endif
        A.unsafeWrite marr i lo
        A.unsafeWrite marr (i+1) hi
        return 2
    where n = ord c
          m = n - 0x10000
          lo = fromIntegral $ (m `shiftR` 10) + 0xD800
          hi = fromIntegral $ (m .&. 0x3FF) + 0xDC00
{-# INLINE unsafeWrite #-}


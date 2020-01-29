{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, BangPatterns, UnboxedTuples #-}

module SDP.Text
(
  -- * Exports
  module SDP.Indexed,
  
  -- * Strict text
  SText, Text
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Indexed  -- just for export

import Data.Text.Array    ( Array (..) )
import Data.Text.Internal ( Text  (..) )

import qualified Data.Text as T

import Data.Maybe
import Data.Bits
import Data.Char

import GHC.Base
  (
    Char (..), Int (..),
    
    shrinkMutableByteArray#, unsafeFreezeByteArray#,
    
    uncheckedIShiftL#, word2Int#, chr#, (+#), (-#)
  )

import GHC.ST ( ST (..), runST )

import GHC.Word ( Word16 (..) )

import SDP.Internal.SBytes
import SDP.Bytes.ST

import Control.Exception.SDP

default ()

-- TODO: move encoding to a separate module.

--------------------------------------------------------------------------------

-- | 'Text' alias, may reduce ambiguity.
type SText = Text

--------------------------------------------------------------------------------

instance Bordered Text Int Char
  where
    lower   _ = 0
    upper  ts = sizeOf ts - 1
    bounds ts = (0, sizeOf ts - 1)
    sizeOf    = T.length

instance Linear Text Char
  where
    isNull = T.null
    lzero  = T.empty
    single = T.singleton
    
    uncons = fromMaybe (pfailEx "(:>)") . T.uncons
    unsnoc = fromMaybe (pfailEx "(:<)") . T.unsnoc
    toHead = T.cons
    toLast = T.snoc
    
    (++) = T.append
    head = T.head
    last = T.last
    tail = T.tail
    init = T.init
    
    replicate n e = T.replicate n (T.singleton e)
    
    fromList = T.pack
    reverse  = T.reverse
    
    listL = T.unpack
    listR = T.unpack . reverse
    
    concat = T.concat . toList
    filter = T.filter
    
    concatMap f = concat . foldr ((:) . f) []
    intersperse = T.intersperse
    partition   = T.partition

instance Split Text Char
  where
    take  = T.take
    drop  = T.drop
    split = T.splitAt
    
    -- TODO: implement splits
    
    -- TODO: implement parts
    
    chunks = T.chunksOf
    
    isPrefixOf = T.isPrefixOf
    isSuffixOf = T.isSuffixOf
    isInfixOf  = T.isInfixOf
    
    prefix p = T.foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = T.foldl (\ c e -> p e ? c + 1 $ 0) 0
    
    takeWhile = T.takeWhile
    dropWhile = T.dropWhile
    
    takeEnd = T.takeWhileEnd
    dropEnd = T.dropWhileEnd

instance Indexed Text Int Char
  where
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    fromIndexed  es = runST $ fromIndexed' es >>= done
    
    Z  // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = runST $ thaw es >>= (`overwrite` ascs) >>= done
    
    -- | O(n).
    (!^) = T.index
    
    -- | O(n).
    (.!) = T.index
    
    (*$) = undefined

--------------------------------------------------------------------------------

instance Thaw (ST s) Text (STBytes# s Char)
  where
    thaw es@(Text _ _ m) = do
        marr <- filled n '\0'
        unzip# (textRepack es) m marr n
      where
        n = sizeOf es

instance Thaw (ST s) Text (STBytes s Int Char)
  where
    thaw es = STBytes l u <$> thaw es where (l, u) = defaultBounds (sizeOf es)

instance Freeze (ST s) (STBytes# s Char) Text
  where
    freeze = copied >=> unsafeFreeze
    
    unsafeFreeze es = getSizeOf es >>= zip# es

instance (Index i) => Freeze (ST s) (STBytes s i Char) Text
  where
    freeze = copied >=> unsafeFreeze
    
    unsafeFreeze (STBytes l u marr#) = marr# `zip#` size (l, u)

--------------------------------------------------------------------------------

{-
  @SDP@ structures (Bytes\#, Bytes, Ublist, ByteList) store characters
  pessimistically (by 32 bit), which makes random access possible.
  @Text@ stores data more tightly and prefer stream access.
-}

zip# :: STBytes# s Char -> Int -> ST s Text
zip# es n = go 0 0
  where
    go i j@(I# j#) = if i < n
      then do c <- es !#> i; o <- write# es' c j; go (i + 1) (j + o)
      
      else ST $ \ s1# -> case shrinkMutableByteArray# marr# j# s1# of
        s2# -> case unsafeFreezeByteArray# marr# s2# of
          (# s3#, text# #) -> (# s3#, Text (Array text#) 0 j #)
    
    marr# = unsafeUnpackMutableBytes# es
    es'   = unsafeCoerceMutableBytes# es -- [safe]: Char => Word16

unzip# :: SBytes# Word16 -> Int -> STBytes# s Char -> Int -> ST s (STBytes# s Char)
unzip# src n marr m = do go (n - 1) (m - 1); return marr
  where
    go i j = when (i > 0) $ do o <- move src i marr j; go (i - 2) (j - o)

--------------------------------------------------------------------------------

{-# INLINE write# #-}
write# :: STBytes# s Word16 -> Char -> Int -> ST s Int
write# es c i = if n < 0x10000
    then do writeM_ es i c'; return 1
    else do writeM_ es i lo; writeM_ es (i + 1) hi; return 2
  where
    n  = ord c
    m  = n - 0x10000
    c' = fromIntegral n
    lo = fromIntegral $ (m `shiftR` 10) + 0xD800
    hi = fromIntegral $ (m  .&.  0x3FF) + 0xDC00

move :: SBytes# Word16 -> Int -> STBytes# s Char -> Int -> ST s Int
move src i marr j = if lo >= 0xD800 && lo <= 0xDBFF
    then do writeM_ marr j (u16c lo hi); return 1
    else do writeM_ marr (j - 1) (w2c lo); writeM_ marr j (w2c hi); return 2
  where
    lo = src !^ (i - 1)
    hi = src !^ i

--------------------------------------------------------------------------------

-- Pack 'Text' as SBytes# without representation changes.
{-# INLINE textRepack #-}
textRepack :: Text -> SBytes# Word16
textRepack (Text (Array text#) o n) = drop o (unsafePackPseudoBytes# n text#)

{-# INLINE done #-}
done :: STBytes# s Char -> ST s Text
done =  unsafeFreeze

{-# INLINE u16c #-}
u16c :: Word16 -> Word16 -> Char
u16c (W16# a#) (W16# b#) = C# (chr# (upper# +# lower# +# 0x10000#))
  where
    !upper# = uncheckedIShiftL# (word2Int# a# -# 0xD800#) 10#
    !lower# = word2Int# b# -# 0xDC00#

{-# INLINE w2c #-}
w2c :: Word16 -> Char
w2c (W16# w#) = C# (chr# (word2Int# w#))

pfailEx :: String -> a
pfailEx msg = throw $ PatternMatchFail $ "in SDP.Text." ++ msg




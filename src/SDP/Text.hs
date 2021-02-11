{-# LANGUAGE Trustworthy, MagicHash, BangPatterns, UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.Text
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC only)
    
    @SDP.Text@ provides SDP instances for strict 'Text'.
-}
module SDP.Text
(
  -- * Exports
  module System.IO.Classes,
  module SDP.IndexedM,
  
  -- * Strict text
  SText, Text, T.toCaseFold, T.toLower, T.toUpper, T.toTitle
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM

import SDP.Prim.SBytes

import Data.Text.Internal ( Text  (..) )
import Data.Text.Array    ( Array (..) )

import Data.Text.Internal.Fusion ( Stream (..), Step (..), stream )
import qualified Data.Text.IO as IO
import qualified Data.Text as T

import Data.Coerce
import Data.Maybe
import Data.Bits
import Data.Char

import GHC.Base
  (
    Char (..), Int (..),
    
    shrinkMutableByteArray#, unsafeFreezeByteArray#,
    
    uncheckedIShiftL#, word2Int#, chr#, (+#), (-#)
  )

import GHC.Word ( Word16 (..) )
import GHC.ST   ( ST (..) )

import System.IO.Classes

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'Text' alias, may reduce ambiguity.
type SText = Text

--------------------------------------------------------------------------------

{- Nullable and Estimate instances. -}

instance Nullable Text
  where
    isNull = T.null
    lzero  = T.empty

instance Estimate Text
  where
    {-# INLINE (<.=>) #-}
    (<.=>) = T.compareLength
    
    {-# INLINE (<==>) #-}
    xs <==> ys = xs `T.compareLength` sizeOf ys

--------------------------------------------------------------------------------

{- Bordered, Linear and Split instances. -}

instance Bordered Text Int
  where
    lower   _ = 0
    upper  ts = sizeOf ts - 1
    bounds ts = (0, sizeOf ts - 1)
    sizeOf    = T.length

instance Linear Text Char
  where
    uncons' = T.uncons
    unsnoc' = T.unsnoc
    
    uncons = fromMaybe (pfailEx "(:>)") . T.uncons
    unsnoc = fromMaybe (pfailEx "(:<)") . T.unsnoc
    single = T.singleton
    toHead = T.cons
    toLast = T.snoc
    
    (++) = T.append
    (!^) = T.index
    head = T.head
    last = T.last
    tail = T.tail
    init = T.init
    
    write es = (es //) . single ... (,)
    
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
    
    ofoldr f base = fold' . stream
      where
        fold' (Stream nxt s0 _) = go 0 s0
          where
            go !i !s = case nxt s of
              Yield x s' -> f i x (go (i + 1) s')
              Skip    s' -> go i s'
              Done       -> base
    
    ofoldl f base' = fold' . stream
      where
        fold' (Stream nxt s0 _) = go base' 0 s0
          where
            go base !i !s = case nxt s of
              Yield x s' -> go (f i base x) (i + 1) s'
              Skip    s' -> go base i s'
              Done       -> base
    
    o_foldr = T.foldr
    o_foldl = T.foldl

instance Split Text Char
  where
    take  = T.take
    drop  = T.drop
    keep  = T.takeEnd
    sans  = T.dropEnd
    split = T.splitAt
    
    splitsBy = T.split
    splitsOn = T.splitOn
    
    replaceBy = T.replace
    chunks    = T.chunksOf
    
    isPrefixOf = T.isPrefixOf
    isSuffixOf = T.isSuffixOf
    isInfixOf  = T.isInfixOf
    
    justifyL = T.justifyLeft
    justifyR = T.justifyRight
    
    prefix p = T.foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = T.foldl (\ c e -> p e ? c + 1 $ 0) 0
    
    takeWhile = T.takeWhile
    dropWhile = T.dropWhile
    
    takeEnd = T.takeWhileEnd
    dropEnd = T.dropWhileEnd

--------------------------------------------------------------------------------

{- Map and Indexed instances. -}

instance Map Text Int Char
  where
    toMap ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    toMap' defvalue ascs = isNull ascs ? Z $ assoc' (l, u) defvalue ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    Z  // ascs = toMap ascs
    es // ascs = runST $ thaw es >>= (`overwrite` ascs) >>= done
    
    (.!) = T.index
    
    kfoldr = ofoldr
    kfoldl = ofoldl

instance Indexed Text Int Char
  where
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es = runST $ fromIndexed' es >>= done

--------------------------------------------------------------------------------

instance Thaw (ST s) Text (STBytes# s Char)
  where
    thaw es = filled (sizeOf es) '\0' >>= unzip# (textRepack es)

instance Freeze (ST s) (STBytes# s Char) Text
  where
    unsafeFreeze = zip#
    freeze       = copied >=> unsafeFreeze

instance (MonadIO io) => Thaw io Text (MIOBytes# io Char)
  where
    unsafeThaw = pack' . unsafeThaw
    thaw       = pack' . thaw

instance (MonadIO io) => Freeze io (MIOBytes# io Char) Text
  where
    unsafeFreeze (MIOBytes# es) = stToMIO (unsafeFreeze es)
    freeze       (MIOBytes# es) = stToMIO (freeze es)

--------------------------------------------------------------------------------

{- IsFile and IsTextFile instances. -}

instance IsFile Text
  where
    hGetContents = liftIO  .  IO.hGetContents
    hPutContents = liftIO ... IO.hPutStr

instance IsTextFile Text
  where
    hPutStrLn = liftIO ... IO.hPutStrLn
    hGetLine  = liftIO  .  IO.hGetLine
    hPutStr   = liftIO ... IO.hPutStr

--------------------------------------------------------------------------------

{-
  Note:
  @SDP@ structures (Bytes#, Bytes, Ublist, ByteList) stores characters
  pessimistically (by 32 bit), and provides random access. @Text@ stores data
  more tightly and prefer stream access.
-}
zip# :: STBytes# s Char -> ST s Text
zip# es = go o o
  where
    go i j@(I# j#) = if i < n
      then do c <- es !#> i; o' <- write# es' c j; go (i + 1) (j + o')
      
      else ST $ \ s1# -> case shrinkMutableByteArray# marr# j# s1# of
        s2# -> case unsafeFreezeByteArray# marr# s2# of
          (# s3#, text# #) -> (# s3#, Text (Array text#) 0 j #)
    
    marr# = unpackSTBytes# es
    es'   = unsafeCoerceSTBytes# es -- [safe]: Char => Word16
    
    o = I# (offsetSTBytes# es)
    n = sizeOf es

unzip# :: SBytes# Word16 -> STBytes# s Char -> ST s (STBytes# s Char)
unzip# src marr = do go 0 0; return marr
  where
    go i j = when (i < sizeOf src) $ if lo >= 0xD800 && lo <= 0xDBFF
       then do writeM marr j (u16c lo hi); go (i + 2) (j + 1)
       else do writeM marr j   (w2c lo);   go (i + 1) (j + 1)
      where
        lo = src !^ i
        hi = src !^ (i + 1)

write# :: STBytes# s Word16 -> Char -> Int -> ST s Int
write# es c i = if n < 0x10000
    then do writeM es i c'; return 1
    else do writeM es i lo; writeM es (i + 1) hi; return 2
  where
    n  = ord c
    m  = n - 0x10000
    c' = fromIntegral n
    lo = fromIntegral $ (m `shiftR` 10) + 0xD800
    hi = fromIntegral $ (m  .&.  0x3FF) + 0xDC00

--------------------------------------------------------------------------------

pack' :: (MonadIO io) => ST RealWorld (STBytes# RealWorld e) -> io (MIOBytes# io e)
pack' =  stToMIO . coerce

-- Pack 'Text' as SBytes# without representation changes.
{-# INLINE textRepack #-}
textRepack :: Text -> SBytes# Word16
textRepack (Text (Array text#) o n) = drop o (packSBytes# n text#)

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
pfailEx =  throw . PatternMatchFail . showString "in SDP.Text."



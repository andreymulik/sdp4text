{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, BangPatterns, UnboxedTuples #-}

{- |
    Module      :  SDP.Text.Lazy
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC only)
    
    @SDP.Text.Lazy@ provides SDP instances for lazy 'Text'.
-}
module SDP.Text.Lazy
(
  -- * Exports
  module SDP.Indexed,
  
  -- * Lazy text
  LText, Text
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Indexed  -- just for export

import qualified Data.Text.Lazy as L

import SDP.Text ()

import Data.Text.Lazy ( Text )

import Data.Maybe

import SDP.ByteList.STUblist
import SDP.ByteList.ST

import SDP.Internal.SBytes

import Control.Exception.SDP
import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | 'Text' alias, may reduce ambiguity.
type LText = Text

--------------------------------------------------------------------------------

instance Estimate Text
  where
    {-# INLINE (<.=>) #-}
    xs <.=> n = xs `L.compareLength` fromIntegral n
    
    {-# INLINE (<==>) #-}
    xs <==> ys = xs `L.compareLength` L.length ys

--------------------------------------------------------------------------------

instance Bordered Text Int Char
  where
    lower   _ = 0
    upper  ts = sizeOf ts - 1
    bounds ts = (0, sizeOf ts - 1)
    sizeOf    = fromIntegral . L.length

instance Linear Text Char
  where
    isNull = L.null
    lzero  = L.empty
    single = L.singleton
    
    uncons = fromMaybe (pfailEx "(:>)") . L.uncons
    unsnoc = fromMaybe (pfailEx "(:<)") . L.unsnoc
    toHead = L.cons
    toLast = L.snoc
    
    (++) = L.append
    head = L.head
    last = L.last
    tail = L.tail
    init = L.init
    
    replicate n e = L.replicate (fromIntegral n) (L.singleton e)
    
    fromList = L.pack
    reverse  = L.reverse
    
    listL = L.unpack
    listR = L.unpack . reverse
    
    concat = L.concat . toList
    filter = L.filter
    
    concatMap f = concat . foldr ((:) . f) []
    intersperse = L.intersperse
    partition   = L.partition

instance Split Text Char
  where
    take   = L.take     . fromIntegral
    drop   = L.drop     . fromIntegral
    split  = L.splitAt  . fromIntegral
    chunks = L.chunksOf . fromIntegral
    
    isPrefixOf = L.isPrefixOf
    isSuffixOf = L.isSuffixOf
    isInfixOf  = L.isInfixOf
    
    prefix p = L.foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = L.foldl (\ c e -> p e ? c + 1 $ 0) 0
    
    takeWhile = L.takeWhile
    dropWhile = L.dropWhile
    takeEnd   = L.takeWhileEnd
    dropEnd   = L.dropWhileEnd

--------------------------------------------------------------------------------

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
    (!^) es = L.index es . fromIntegral
    
    -- | O(n).
    (.!) es = L.index es . fromIntegral

--------------------------------------------------------------------------------

instance Thaw (ST s) Text (STUblist s Char)
  where
    thaw       = foldr (liftA2 STUblist .       thaw) (return STUBEmpty) . L.toChunks
    unsafeThaw = foldr (liftA2 STUblist . unsafeThaw) (return STUBEmpty) . L.toChunks

instance Thaw (ST s) Text (STByteList s Int Char)
  where
    thaw es = STByteList l u <$> thaw es
      where
        (l, u) = defaultBounds (sizeOf es)
    
    unsafeThaw es = STByteList l u <$> unsafeThaw es
      where
        (l, u) = defaultBounds (sizeOf es)

instance Freeze (ST s) (STUblist s Char) Text
  where
    freeze       = fmap L.fromChunks . mapM    freeze    . chunks'
    unsafeFreeze = fmap L.fromChunks . mapM unsafeFreeze . chunks'

instance Freeze (ST s) (STByteList s Int Char) Text
  where
    freeze       (STByteList _ _ es) = freeze es
    unsafeFreeze (STByteList _ _ es) = unsafeFreeze es

--------------------------------------------------------------------------------

chunks' :: STUblist s Char -> [STBytes# s Char]
chunks' es = case es of {STUblist marr# marr -> marr# : chunks' marr; _ -> []}

done :: STUblist s Char -> ST s Text
done =  freeze

pfailEx :: String -> a
pfailEx msg = throw $ PatternMatchFail $ "in SDP.Text." ++ msg






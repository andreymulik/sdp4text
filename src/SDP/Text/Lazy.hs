{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MagicHash, Unsafe #-}

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
  module System.IO.Classes,
  
  module SDP.IndexedM,
  
  -- * Lazy text
  LText, Text
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as IO

import Data.Text.Lazy ( Text )
import SDP.Text ()

import Data.Maybe

import SDP.Templates.AnyChunks
import SDP.ByteList.STUblist

import Control.Exception.SDP
import Control.Monad.ST

import System.IO.Classes

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

instance Bordered Text Int
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
    
    fromIndexed es = runST $ fromIndexed' es >>= done
    
    Z  // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = runST $ thaw es >>= (`overwrite` ascs) >>= done
    
    (!^) es = L.index es . fromIntegral
    (.!) es = L.index es . fromIntegral

--------------------------------------------------------------------------------

instance Thaw (ST s) Text (STUblist s Char)
  where
    unsafeThaw = fmap AnyChunks . mapM unsafeThaw . L.toChunks
    thaw       = fmap AnyChunks . mapM thaw       . L.toChunks

instance Freeze (ST s) (STUblist s Char) Text
  where
    unsafeFreeze (AnyChunks es) = L.fromChunks <$> mapM unsafeFreeze es
    freeze       (AnyChunks es) = L.fromChunks <$> mapM freeze       es

--------------------------------------------------------------------------------

{- IsFile and IsTextFile instances. -}

instance IsFile Text
  where
    hGetContents = IO.hGetContents
    hPutContents = IO.hPutStr

instance IsTextFile Text
  where
    hPutStrLn = IO.hPutStrLn
    hGetLine  = IO.hGetLine
    hPutStr   = IO.hPutStr

--------------------------------------------------------------------------------

done :: STUblist s Char -> ST s Text
done =  freeze

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Text.Lazy."


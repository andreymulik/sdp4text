{-# LANGUAGE Safe, CPP, MagicHash, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.Text.Lazy
    Copyright   :  (c) Andrey Mulik 2020-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Text.Lazy" provides @sdp@ instances for lazy 'Text'.
-}
module SDP.Text.Lazy
(
  -- * Exports
  module System.IO.Classes,
  
  module SDP.IndexedM,
  
  -- * Lazy text
  LText, Text, L.toCaseFold, L.toLower, L.toUpper, L.toTitle,
  L.fromChunks, L.toChunks, L.toStrict, L.fromStrict,
  L.foldrChunks, L.foldlChunks
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.Text ()

import qualified Data.Text.Lazy.IO as IO
import qualified Data.Text.Lazy as L

import Data.Text.Lazy ( Text )
import Data.Maybe

import SDP.Templates.AnyChunks
import SDP.ByteList.IOUblist
import SDP.ByteList.STUblist

import Control.Exception.SDP

import System.IO.Classes

default ()

--------------------------------------------------------------------------------

-- | 'Text' alias, may reduce ambiguity.
type LText = Text

--------------------------------------------------------------------------------

{- Nullable, Forceable and Estimate instances. -}

instance Nullable Text where isNull = L.null; lzero = L.empty

#if MIN_VERSION_sdp(0,3,0)
instance Forceable Text where force = L.fromChunks . map force . L.toChunks
#endif

instance Estimate Text
  where
    {-# INLINE (<.=>) #-}
    xs <.=> n = xs `L.compareLength` fromIntegral n
    
    {-# INLINE (<==>) #-}
    xs <==> ys = xs `L.compareLength` L.length ys

--------------------------------------------------------------------------------

{- Bordered, Linear and Split instances. -}

instance Bordered Text Int
  where
    lower    _ = 0
    upper   ts = sizeOf ts - 1
    bounds  ts = (0, sizeOf ts - 1)
    sizeOf     = fromEnum . L.length
#if MIN_VERSION_sdp(0,3,0)
    rebound    = take . size
#endif

instance Linear Text Char
  where
    uncons' = L.uncons
#if MIN_VERSION_text(1,2,3)
    unsnoc' = L.unsnoc
#else
    unsnoc' = L.null ?- \ t -> (L.init t, L.last t)
#endif
    uncons = fromMaybe (pfailEx "(:>)") . L.uncons
    unsnoc = fromMaybe (pfailEx "(:<)") . unsnoc'
    single = L.singleton
    toHead = L.cons
    toLast = L.snoc
    
    (++) = L.append
    head = L.head
    last = L.last
    tail = L.tail
    init = L.init
    
    (!^) es = L.index es . fromIntegral
    
    write es = (es //) . single ... (,)
    
    replicate n e = L.replicate (fromIntegral n) (L.singleton e)
    
    fromList = L.pack
    reverse  = L.reverse
    
    listR = L.unpack . reverse
    listL = L.unpack
    
    concat = L.concat . toList
    filter = L.filter
    
#if !MIN_VERSION_sdp(0,3,0)
    force = L.fromChunks . map force . L.toChunks
#endif

    concatMap f = concat . foldr ((:) . f) []
    intersperse = L.intersperse
    partition   = L.partition
    
    ofoldr f =
      let go = \ ch (i, acc) -> (i + sizeOf ch, ofoldr (f . (+ i)) acc ch)
      in  snd ... L.foldrChunks go . (,) 0
    
    ofoldl f base text =
      let go = \ (i, acc) ch -> (i + sizeOf ch, ofoldl (f . (+ i)) acc ch)
      in  snd $ L.foldlChunks go (upper text, base) text
    
    o_foldl' = L.foldl'
    o_foldr  = L.foldr
    o_foldl  = L.foldl
#if !MIN_VERSION_sdp(0,3,0)
instance Split Text Char
  where
#endif
    take   = L.take     . fromIntegral
    drop   = L.drop     . fromIntegral
    keep   = L.takeEnd  . fromIntegral
    sans   = L.dropEnd  . fromIntegral
    split  = L.splitAt  . fromIntegral
    chunks = L.chunksOf . fromIntegral
    
    replaceBy = L.replace
    splitsOn  = L.splitOn
    splitsBy  = L.split
    
    justifyL = L.justifyLeft  . fromIntegral
    justifyR = L.justifyRight . fromIntegral
    
    isPrefixOf = L.isPrefixOf
    isSuffixOf = L.isSuffixOf
    isInfixOf  = L.isInfixOf
    
    takeWhile = L.takeWhile
    dropWhile = L.dropWhile
    takeEnd   = L.takeWhileEnd
    dropEnd   = L.dropWhileEnd

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
    
    (.!) es = L.index es . fromIntegral
    
    kfoldr' = ofoldr'
    kfoldl' = ofoldl'
    kfoldr  = ofoldr
    kfoldl  = ofoldl

instance Indexed Text Int Char
  where
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es = runST $ fromIndexed' es >>= done

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance Thaw (ST s) Text (STUblist s Char)
  where
    unsafeThaw = fromChunksM <=< mapM unsafeThaw . L.toChunks
    thaw       = fromChunksM <=< mapM thaw       . L.toChunks

instance Freeze (ST s) (STUblist s Char) Text
  where
    unsafeFreeze = fmap L.fromChunks . mapM unsafeFreeze . toChunks
    freeze       = fmap L.fromChunks . mapM freeze       . toChunks

instance (MonadIO io) => Thaw io Text (MIOUblist io Char)
  where
    unsafeThaw = fromChunksM <=< mapM unsafeThaw . L.toChunks
    thaw       = fromChunksM <=< mapM thaw       . L.toChunks

instance (MonadIO io) => Freeze io (MIOUblist io Char) Text
  where
    unsafeFreeze = fmap L.fromChunks . mapM unsafeFreeze . toChunks
    freeze       = fmap L.fromChunks . mapM freeze       . toChunks

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

done :: STUblist s Char -> ST s Text
done =  freeze

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Text.Lazy."


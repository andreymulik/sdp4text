{-# LANGUAGE MultiParamTypeClasses #-}

{- |
    Module      :  SDP.Text.Builder
    Copyright   :  (c) Andrey Mulik 2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable module)
    
    @SDP.Text.Builder@ provides SDP instances for text 'Builder'.
    
    Note that 'Builder' is a service type for efficient @Text@ creation which
    isn't intended for element-wise operations and content changes. 'Linear'
    instance provided for convenience and many functions (folds, selections,
    etc.) create intermediate structures (text, string).
-}
module SDP.Text.Builder
(
  -- * Export
  module SDP.Linear,
  
  -- * Builder
  Builder, fromText, toLazyText, fromLazyText, flush
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Text.Lazy
import SDP.Linear

import Data.Text.Lazy.Builder

default ()

--------------------------------------------------------------------------------

instance Nullable Builder
  where
    isNull = (== mempty)
    lzero  = mempty

instance Linear Builder Char
  where
    fromFoldable = fromLazyText . fromFoldable
    fromListN    = fromLazyText ... fromListN
    replicate    = fromText ... replicate
    fromList     = fromString
    single       = singleton
    
    toHead e es = singleton e <> es
    toLast es e = es <> singleton e
    
    listL = listL . toLazyText
    listR = listR . toLazyText
    (++)  = (<>)
    
    reverse = fromLazyText . reverse . toLazyText
    
    concatMap = foldMap
    concat    = fold
    
    force = fromLazyText . toLazyText
    
    tail = fromLazyText . tail . toLazyText
    init = fromLazyText . init . toLazyText
    head = head . toLazyText
    last = last . toLazyText
    
    partition   f = both fromLazyText . partition f . toLazyText
    intersperse e = fromLazyText . intersperse e . toLazyText
    filter      p = fromLazyText . filter p . toLazyText
    
    nubBy f = fromLazyText . nubBy f . toLazyText
    nub     = fromLazyText .   nub   . toLazyText
    
    ofoldr  f base = ofoldr  f base . toLazyText
    ofoldl  f base = ofoldl  f base . toLazyText
    ofoldr' f base = ofoldr' f base . toLazyText
    ofoldl' f base = ofoldl' f base . toLazyText
    
    o_foldr  f base = o_foldr  f base . toLazyText
    o_foldl  f base = o_foldl  f base . toLazyText
    o_foldr' f base = o_foldr' f base . toLazyText
    o_foldl' f base = o_foldl' f base . toLazyText

--------------------------------------------------------------------------------

instance IsFile Builder
  where
    hGetContents     = fmap fromLazyText . hGetContents
    hPutContents hdl = hPutContents hdl . toLazyText

instance IsTextFile Builder
  where
    hGetLine      = fmap fromLazyText . hGetLine
    hPutStrLn hdl = hPutStrLn hdl . toLazyText
    hPutStr   hdl = hPutStr   hdl . toLazyText



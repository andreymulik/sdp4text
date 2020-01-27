{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module SDP.Text
(
  -- * Exports
  module SDP.Indexed,
  
  -- * Strict text
  Text
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed

import Data.Text ( Text )
import qualified Data.Text as T

import Data.Maybe

import Control.Exception.SDP

default ()

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
    
    -- TODO: implement replicate
    
    fromList = T.pack
    reverse  = T.reverse
    
    listL = T.unpack
    listR = T.unpack . reverse
    
    concat = T.concat . toList
    filter = T.filter
    
    -- TODO: implement concatMap
    
    intersperse = T.intersperse
    partition   = T.partition

--------------------------------------------------------------------------------

pfailEx :: String -> a
pfailEx msg = throw $ PatternMatchFail $ "in SDP.Text." ++ msg




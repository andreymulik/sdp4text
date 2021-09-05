module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.Text.Lazy

import Test.SDP

import Test.QuickCheck.Instances ()

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- common tests
    testProperty "lazy-text-eq             " eqProp,
    testProperty "lazy-text-ord            " ordProp,
    testProperty "lazy-text-lexicographic  " lgoProp,
    
    -- linear tests
    testProperty "lazy-text-linear-basic   " basicLinearProp,
    testProperty "lazy-text-linear-decons  " deconstructionLinearProp,
    testProperty "lazy-text-linear-cons    " constructionLinearProp,
    testProperty "lazy-text-linear-reverse " reverseProp,
    testProperty "lazy-text-linear-concat  " concatProp,
    
    -- split test
    testProperty "lazy-text-split          " splitProp,
    
    -- indexed tests
    testProperty "lazy-text-indexed-basic  " basicIndexedProp,
    testProperty "lazy-text-indexed-assoc  " assocIndexedProp,
    testProperty "lazy-text-indexed-read   " readIndexedProp,
    
    -- estimate test
    testProperty "lazy-text-estimate       " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqProp :: TestEq Text
eqProp =  eqTest

--------------------------------------------------------------------------------

{- Ord property. -}

ordProp :: TestOrd Text
ordProp =  ordTest

lgoProp :: Long Text -> Long Text -> Bool
lgoProp (Long xs) (Long ys) = lexicographicOrdTest xs ys

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp          :: Char -> Text -> Bool
basicLinearProp          =  basicLinearTest

deconstructionLinearProp :: Text -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp   :: Char -> Text -> Bool
constructionLinearProp   =  constructionLinearTest

reverseProp              :: Text -> Bool
reverseProp              =  reverseTest

replicateProp            :: TestLinear Text Char
replicateProp            =  replicateTest

concatProp               :: Text -> Bool
concatProp               =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: Char -> TestSplit Text
splitProp =  splitTest . (>)

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed Text Int
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed Text Int
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed Text Int
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate Text
estimateProp =  estimateTest




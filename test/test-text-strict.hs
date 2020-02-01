module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.Text

import Test.SDP

import Test.QuickCheck.Instances.Text ()

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- common tests
    testProperty "strict-text-eq             " eqProp,
    testProperty "strict-text-ord            " ordProp,
    testProperty "strict-text-lexicographic  " lgoProp,
    
    -- linear tests
    testProperty "strict-text-linear-basic   " basicLinearProp,
    testProperty "strict-text-linear-decons  " deconstructionLinearProp,
    testProperty "strict-text-linear-cons    " constructionLinearProp,
    testProperty "strict-text-linear-reverse " reverseProp,
    testProperty "strict-text-linear-concat  " concatProp,
    
    -- split test
    testProperty "strict-text-split          " splitProp,
    
    -- indexed tests
    testProperty "strict-text-indexed-basic  " basicIndexedProp,
    testProperty "strict-text-indexed-assoc  " assocIndexedProp,
    testProperty "strict-text-indexed-read   " readIndexedProp,
    
    -- estimate test
    testProperty "strict-text-estimate       " estimateProp
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

splitProp :: TestSplit Text
splitProp =  splitTest

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




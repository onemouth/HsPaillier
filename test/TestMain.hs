{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Paillier


instance Show (IO (PubKey, PrvKey))

instance Arbitrary (IO (PubKey, PrvKey)) where
    arbitrary = do
        nBits <- frequency [(3,return 256), (2,return 512), (1,return 1024)]
        return $ genKey nBits

prop_trival :: IO (PubKey, PrvKey) -> Property 
prop_trival keys = monadicIO $ do 
    (pub, prv) <- run keys
    assert $ True

main :: IO ()
main = $(defaultMainGenerator)

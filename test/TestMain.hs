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
        nBits <- frequency [(2,return 512), (1,return 1024)]
        return $ genKey nBits


prop_fixed_plaintext :: IO (PubKey, PrvKey) -> Property 
prop_fixed_plaintext keys = monadicIO $ do 
    (pub, prv) <- run keys
    let plaintext = 37
    ciphertext <- run $ encrypt pub plaintext
    let plaintext' = decrypt prv pub ciphertext
    assert $ plaintext == plaintext'

main :: IO ()
main = $(defaultMainGenerator)

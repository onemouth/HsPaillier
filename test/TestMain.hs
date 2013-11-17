{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.Framework.TH
import Paillier


test_trivial = TestCase $ do
    (pub, prv) <- genKey 1024
    assertFailure "test"
    return ()

main = $(defaultMainGenerator)

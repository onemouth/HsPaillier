module Paillier.Internal.Type
    ( PlainText
    , CipherText
    , PubKey
    , PrvKey) where

newtype PlainText = PlainText Integer

newtype CipherText = CipherText Integer

data PubKey = PubKey{  bits :: Int
                     , n :: Integer
                    } deriving (Show)

data PrvKey = PrvKey{  lambda :: Integer
                     , x :: Integer
                    } deriving (Show)

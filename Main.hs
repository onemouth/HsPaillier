{-# LANGUAGE DeriveDataTypeable #-}

module Main where
import System.Console.CmdArgs
import qualified Paillier as P

data Sample = Sample {encrypt :: Integer}
    deriving (Show, Data, Typeable)

sample = Sample {encrypt = def &= help "which integer to encrypt" &= typ "0"}


main = do
    print =<< cmdArgs sample
    

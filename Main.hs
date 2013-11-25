{-# LANGUAGE DeriveDataTypeable #-}

module Main where
import System.Console.CmdArgs
import qualified Paillier as P

data Sample = Sample {encrypt :: Integer}
    deriving (Show, Data, Typeable)

sample = Sample {encrypt = def &= help "which integer to encrypt" &= typ "0"}


main = do
    --print =<< cmdArgs sample
    (pubKey, prvKey) <- P.genKey 2048
    print (pubKey, prvKey)
    let p = 1 :: Integer
    c <- P.encrypt pubKey p
    putStrLn $ "ciphertext: " ++ show c
    putStrLn $ "plaintext: " ++ show (P.decrypt prvKey pubKey c)
    

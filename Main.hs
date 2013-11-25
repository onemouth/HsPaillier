{-# LANGUAGE DeriveDataTypeable #-}

module Main where
import System.Console.CmdArgs
import qualified Paillier as P

data Sample = Sample {encrypt :: Integer}
    deriving (Show, Data, Typeable)

sample = Sample {encrypt = def &= help "which integer to encrypt" &= typ "0"}


main = do
    --print =<< cmdArgs sample
    (pubKey, prvKey) <- P.genKey 256
    print (pubKey, prvKey)
    let p = 2 :: Integer
    c <- P.encrypt pubKey p
    putStrLn $ "ciphertext: " ++ show c
    --putStrLn $ "plaintext: " ++ show (P.decrypt prvKey pubKey c)
    let p' = 100 :: Integer
    c' <- P.encrypt pubKey p'
    putStrLn $ "ciphertext: " ++ show c'
    let c'' = P.cipherMul pubKey c c'
    putStrLn $ "plaintext: " ++ show (P.decrypt prvKey pubKey c'')
    

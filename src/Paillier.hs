{-# LANGUAGE CPP #-}
module Paillier where

import Data.Maybe
import Crypto.Random
import Crypto.Number.Prime
import Crypto.Number.Generate (generateOfSize)
import Crypto.Number.ModArithmetic

#if 1
import System.IO
#endif

type PlainText = Integer 

type CipherText = Integer

data PubKey = PubKey{  bits :: Int  -- ^ e.g., 2048
                     , n_modulo :: Integer -- ^ n = pq
                     , g :: Integer -- ^ g = n+1
                     , n_square :: Integer -- ^ n^2
                    } deriving (Show)

data PrvKey = PrvKey{  lambda :: Integer -- ^ lambda(n) = lcm(p-1, q-1)
                     , x :: Integer
                    } deriving (Show)



genKey :: Int -> IO (PubKey, PrvKey)
genKey nBits = do
    -- | choose random primes
    pool <- createEntropyPool
    let rng = cprgCreate pool :: SystemRNG
    let (p, rng1) = generatePrime rng (nBits `div` 2)
    let (q, rng2) = generatePrime rng1 (nBits `div` 2)
    -- | public key parameters
    let modulo = p*q
    let g = modulo+1
    let square = modulo*modulo
    -- | private key parameters
    --let phi_n = (p-1)*(q-1)
    let phi_n = lcm (p-1) (q-1)
    let maybeU = inverse (((expSafe g phi_n square) - 1) `div` modulo) modulo
    --let maybeU = inverse phi_n modulo
    if isNothing maybeU then
       error "genKey failed." 
    else
        return (PubKey{bits=nBits, n_modulo=modulo, g=g, n_square=square}
           ,PrvKey{lambda=phi_n, x=(fromJust maybeU)})

-- | deterministic version of encryption
_encrypt :: PubKey -> PlainText -> Integer -> CipherText
_encrypt pubKey plaintext r = 
    result
    where result = (g_m*r_n) `mod` n_2
          n_2 = n_square pubKey
          g_m = expSafe (g pubKey) plaintext n_2
          r_n = expSafe r (n_modulo pubKey) n_2

gereateR :: SystemRNG -> PubKey -> Integer -> Integer
gereateR rng pubKey guess =
    if guess > (n_modulo pubKey) || (gcd (n_modulo pubKey) guess > 1) then
    --if guess > (n_modulo pubKey) || guess <= 0  then
        gereateR nextRng pubKey nextGuess
    else
        guess

    where (nextGuess, nextRng) = generateOfSize rng (bits pubKey)

encrypt :: PubKey -> PlainText -> IO CipherText
encrypt pubKey plaintext = do
    pool <- createEntropyPool
    let rng = cprgCreate pool :: SystemRNG
#if 1
    hSetBuffering stdout NoBuffering
    putStrLn "get r..."
#endif
    let r = gereateR rng pubKey (n_modulo pubKey)
#if 1
    putStrLn $ "r=" ++ (show r)
#endif
    return $ _encrypt pubKey plaintext r 

decrypt :: PrvKey -> PubKey -> CipherText -> PlainText
decrypt prvKey pubKey ciphertext = 
    let c_lambda = expSafe ciphertext (lambda prvKey) (n_square pubKey)
        l_c_lamdba = (c_lambda - 1) `div` (n_modulo pubKey)
    in  (l_c_lamdba) * (x prvKey) `mod` (n_modulo pubKey)


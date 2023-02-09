{-# LANGUAGE CPP #-}
module Crypto.Paillier where

import Data.Maybe
import Crypto.Random
import Crypto.Number.Prime
import Crypto.Number.Generate (generateBetween)
import Crypto.Number.ModArithmetic

#if 0
import System.IO
#endif

type PlainText = Integer 

type CipherText = Integer

data PubKey = PubKey{  bits :: Int  -- ^ e.g., 2048
                     , nModulo :: Integer -- ^ n = pq
                     , generator :: Integer -- ^ generator = n+1
                     , nSquare :: Integer -- ^ n^2
                    } deriving (Show)

data PrvKey = PrvKey{  lambda :: Integer -- ^ lambda(n) = lcm(p-1, q-1)
                     , x :: Integer
                    } deriving (Show)



genKey :: Int -> IO (PubKey, PrvKey)
genKey nBits = do
    -- choose random primes
    pool <- createEntropyPool
    let rng = cprgCreate pool :: SystemRNG
    let (p, rng1) = generatePrime rng (nBits `div` 2)
    let (q, _) = generatePrime rng1 (nBits `div` 2)
    -- public key parameters
    let modulo = p*q
    let g = modulo+1
    let square = modulo*modulo
    -- private key parameters
    -- let phi_n = (p-1)*(q-1)
    let phi_n = lcm (p-1) (q-1)
    let maybeU = inverse ((expSafe g phi_n square - 1) `div` modulo) modulo
    -- let maybeU = inverse phi_n modulo
    if isNothing maybeU then
       error "genKey failed." 
    else
        return (PubKey{bits=nBits, nModulo=modulo, generator=g, nSquare=square}
           ,PrvKey{lambda=phi_n, x=fromJust maybeU})

-- | deterministic version of encryption
_encrypt :: PubKey -> PlainText -> Integer -> CipherText
_encrypt pubKey plaintext r = 
    result
    where result = (g_m*r_n) `mod` n_2
          n_2 = nSquare pubKey
          g_m = expSafe (generator pubKey) plaintext n_2
          r_n = expSafe r (nModulo pubKey) n_2

generateR :: SystemRNG -> PubKey -> Integer -> Integer
generateR rng pubKey guess =
    if guess >= nModulo pubKey || (gcd (nModulo pubKey) guess > 1) then
        generateR nextRng pubKey nextGuess
    else
        guess

    where (nextGuess, nextRng) = generateBetween rng 1 (nModulo pubKey -1)

encrypt :: PubKey -> PlainText -> IO CipherText
encrypt pubKey plaintext = do
    pool <- createEntropyPool
    let rng = cprgCreate pool :: SystemRNG
#if 0
    hSetBuffering stdout NoBuffering
    putStrLn "get r..."
#endif
    let r = generateR rng pubKey (nModulo pubKey)
#if 0
    putStrLn $ "r=" ++ (show r)
#endif
    return $ _encrypt pubKey plaintext r 

decrypt :: PrvKey -> PubKey -> CipherText -> PlainText
decrypt prvKey pubKey ciphertext = 
    let c_lambda = expSafe ciphertext (lambda prvKey) (nSquare pubKey)
        l_c_lamdba = (c_lambda - 1) `div` nModulo pubKey
    in  l_c_lamdba * x prvKey `mod` nModulo pubKey

-- | ciphetext muliplication is known as homomorphic addition of plaintexts
cipherMul :: PubKey -> CipherText -> CipherText -> CipherText
cipherMul pubKey c1 c2 = c1*c2 `mod` nSquare pubKey

-- | Homomorphic multiplication of plaintexts
-- An encrypted plaintext raised to the power of another plaintext will decrypt to the product of the two plaintexts.
cipherExp :: PubKey -> CipherText -> PlainText -> CipherText
cipherExp pubKey c1 p1 = expSafe c1 p1 (nSquare pubKey)

-- | Homomorphic subtraction => c1 - c2, given two ciphertexts c1 and c2
homoSub :: PubKey -> CipherText -> CipherText -> CipherText
homoSub pubKey c1 c2 = cipherMul pubKey c1 minusc2
  where
    minusc2 = cipherExp pubKey c2 (-1)


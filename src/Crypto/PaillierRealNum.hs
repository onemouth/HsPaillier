module Crypto.PaillierRealNum where

import Crypto.Paillier as P hiding (homoSub)


largenum :: Double
largenum = 2^10

type PT = (Integer, Integer, Integer)

type CT = (CipherText, CipherText, Integer)

e = 0

encodeF :: Double -> PT
encodeF x = (x1, x2, 0)
  where
    (x1, frac) = (floor x, x - fromIntegral (floor x)) :: (Integer, Double)
    x2 = floor (frac * largenum) :: Integer

decodeF :: PT -> Double
decodeF (x1, x2, x3) = (x1D + (x2D/largenum)) / (10^x3)
  where
    x1D = fromInteger x1 :: Double
    x2D = fromInteger x2 :: Double

encrypt :: PubKey -> Double -> IO CT
encrypt pubK z = do
  let (x1, x2, x3) = encodeF z
  x1' <- P.encrypt pubK x1
  x2' <- P.encrypt pubK x2
  return (x1', x2', x3)

decrypt :: PubKey -> PrvKey -> CT -> Double
decrypt pubk prvk (ct1, ct2, i) =
  let (uen1, uen2, x) = (P.decrypt prvk pubk ct1
                        ,P.decrypt prvk pubk ct2
                        ,i
                        )
   in decodeF $ (dealwithNeg uen1, dealwithNeg uen2, x)
   where
     dealwithNeg i = if i >= (maxInt pubk)
                     then i - (nModulo pubk)
                     else i


homoAdd :: PubKey -> CT -> CT -> CT
homoAdd pubk (x1, x2, x3) (y1, y2, y3)
  | x3 == y3  = (cipherMul pubk x1 y1, cipherMul pubk x2 y2, x3)
  | otherwise = if (xdiff > 0)
                then ( cipherExp pubk x1 (10^xdiff)
                     , cipherExp pubk x2 (10^xdiff)
                     , res3
                     )
                else ( cipherExp pubk y1 (10^ydiff)
                     , cipherExp pubk y2 (10^ydiff)
                     , res3
                     )
  where
    res3 = max x3 y3
    xdiff = res3 - x3
    ydiff = res3 - y3

homoSub :: PubKey -> CT -> CT -> CT
homoSub pubk x (y1, y2, y3) =
  homoAdd pubk x (cipherExp pubk y1 (-1), cipherExp pubk y2 (-1), y3)

precision = 6

homoMul :: PubKey -> CT -> Double -> CT
homoMul pubk (x1, x2, x3) y =
  (cipherExp pubk x1 yInt, cipherExp pubk x2 yInt, x3 + precision)
  where
    yInt = floor (y * 10^precision) :: Integer




foo :: IO ()
foo = do
  (pubK, prvK) <- genKey 1024
  let m1 = -42.3
  let m2 = -2.8
  ct1 <- enc pubK m1
  ct2 <- enc pubK m2
  print $ dec pubK prvK $ homoMul pubK ct1 (2.8)
  where
    enc = Crypto.PaillierRealNum.encrypt
    dec = Crypto.PaillierRealNum.decrypt

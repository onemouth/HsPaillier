# HsPaillier


A implenmentation of [Paillier cryptosystem](http://en.wikipedia.org/wiki/Paillier_cryptosystem)

The Paillier cryptosystem has some homomorphic properties.

* Homomorphic addition of plaintexts
* Homomorphic multiplication of plaintexts

Those properties mean that you can manipulate the ciphertext without decrypting it first.

For example, 

    (pubKey, prvKey) <- P.genKey 256 

    let p = 2 :: Integer
    c <- P.encrypt pubKey p
    
    let p' = 100 :: Integer
    c' <- P.encrypt pubKey p'
    
    let c'' = P.cipherMul pubKey c c'
    putStrLn $ "plaintext: " ++ show (P.decrypt prvKey pubKey c'')
    
The result will be 102 which is 2+102 (We just decrypt once!)

The above example is in the `Main.hs`

## How to install

`$ cabal configure`

`$ cabal build`  

`$ cabal test`

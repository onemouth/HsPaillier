{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs
import Crypto.Random

data Sample = Sample {hello :: String}
    deriving (Show, Data, Typeable)

sample = Sample{hello = def}
sample2 = Sample {hello = def &= help "Who to say hello to" &= typ "WORLD"}


main = do
    print =<< cmdArgs sample2
    

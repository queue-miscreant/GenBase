GenBase
=================

Typeclasses for numeric base conversions.
Also provides functionality for bases defined by recurrence relations.

```haskell
import Math.GenBase.Base (tobasei, frombasei, tobasef, frombasef, roundintf)
import Math.GenBase.Recur (Nnacci(..), count)

tobasei 2 10
-- [1,0,1,0] :: [Int]

tobasei (Nnacci 2) 10
-- [1,0,0,1,0] :: [Int]
-- i.e., the Zeckendorf expansion of 10

tobasef (Nnacci 2) 10
-- 10100.01001010101010101010101010101010101010101010101 :: Fp
-- an approximation of the phinary expansion of 10

count (Nnacci 2) !! 10
-- 10100.0101 :: Fp
-- an exact result for the phinary expansion of 10, produced by counting

frombasei 2 [1,0,1,0]
-- 10 :: Int

frombasei (Nnacci 2) [1,0,0,1,0]
-- 10 :: Int

frombasef (Nnacci 2) $ read "10100.0101"
-- 10.0 :: Double

roundintf (Nnacci 2) $ read "10100.0101"
-- Just 10 :: Maybe Int

roundintf (Nnacci 2) $ read "10100"
-- Nothing

```

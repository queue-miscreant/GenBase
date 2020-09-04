--module for positional notation conversions, where the
--place values are either an integer sequence or a real number
{-# LANGUAGE FlexibleInstances #-}
module Math.GenBase.Base (
    Fp (..),
    integ,
    frac,
    plVal,
    prettify,
    zipAdd,
    showTrunc,
    times,
    deficient,

    IntegralBase,
    convertI,
    toBaseI,
    fromBaseI,
    seqcount,
    
    FractionalBase,
    convertF,
    roundingF,
    toBaseF,
    fromBaseF,
    roundInterF
  )
  where

import Prelude
import Control.Monad (guard)

--TODO write leading/trailing 0's ignorer
--generalized floating point place value system
data Fp = Fp {fVect :: [Int], fPlace :: Int} deriving Eq 

--integral and fractional parts of an Fp (before/after the "decimal" point)
integ (Fp a b) = take b a
frac  (Fp a b) = drop b a
--extract the value at place `n`, where the "decimal" point is 0
plVal n (Fp a b) | (b-1-n) < 0 = 0
                 | null rest   = 0
                 | otherwise   = head rest
                   where rest  = drop (b-1-n) a
--convert a list of integrals to an Fp, allowing use of the Num and Show instances
prettify :: Integral a => [a] -> Fp
prettify = (Fp <*> length) . map fromIntegral

--add two lists as if there were trailing zeros in the shorter
zipAdd []     ds     = ds
zipAdd cs     []     = cs
zipAdd (c:cs) (d:ds) = (c+d):(zipAdd cs ds)

instance Num Fp where
  --align the fractional separator
  (+) x@(Fp as a) y@(Fp bs b) = case (a `compare` b) of 
    EQ -> Fp (zipAdd as bs) a
    LT -> y + x
    GT -> Fp (ini ++ zipAdd las bs) a 
      where (ini, las) = splitAt (a-b) as

  --convolution with fractional separator
  --does not account for carries induced by a positional numeral system
  (*) (Fp as a) (Fp bs b) = flip Fp (a+b-1) $ finish $ foldl convolve' ([], []) as
    where convolve' (xs, ys) a = ((a:xs), (sum $ zipWith (*) (a:xs) bs):ys)
          finish (xs, ys) = (reverse ys ++) $ finish' xs $ tail bs
          finish' xs [] = []
          finish' xs ys = (sum $ zipWith (*) xs ys):finish' xs (tail ys)

  --this definition works
  negate (Fp as a) = flip Fp a $ map negate as 
  --but these two might run into some problems
  abs (Fp as a)    = flip Fp a $ map abs as
  signum (Fp as a) = flip Fp a $ map signum as
  fromInteger a    = Fp [fromIntegral a] 1

instance Show Fp where
  show (Fp [] _)     = ""
  show (Fp xs 0)     = '.':(show $ Fp xs (-1))
  show (Fp (x:xs) a) | x < 0     = '-':char:(show $ Fp xs (a-1))
                     | otherwise = char:(show $ Fp xs (a-1))
    where char = toEnum $ (abs x)+fromEnum '0'

showTrunc t fp@(Fp xs a)
           | null s      = show fp
           | otherwise   = (show $ Fp f a) ++ "..."
             where (f,s) = splitAt t xs

--TODO I should really just write a better parser for this...
instance Read Fp where
  readsPrec _ = read' 0 1 0 0 False []
    where done xs 0 = reverse xs
          done xs y = reverse xs ++ (cycle . reverse . take y $ xs)
          read' a b c d neg zs [] = [(Fp (done zs c) a, "")]
          read' a b c d neg zs (x:xs) = case x of
            '.' -> read' a 0 c d False zs xs --fractional part separator
            '-' -> read' a b c d True  zs xs --prime the next digit to interpret as negative
            'r' -> case b of --repeated fractional part by infinite list construction
                  1 -> error "Cannot repeat before point separator"
                  _ -> read' a b c 1 False zs xs
            _   -> read' (a+b) b (c+d) d False (val:zs) xs --interpret char as ASCII digit value
              where val  = if neg then -val' else val'
                    val' = fromEnum x - fromEnum '0'


--integral base typeclass
--Convert an integral number to/from a list of integrals
--the place value of which can be determined by `a`
class IntegralBase a where
  --obtain list of place values
  convertI :: Integral b => a -> [b]
  {-# MINIMAL convertI #-}

  fromBaseI :: Integral b => a -> [Int] -> b
  fromBaseI s = sum . zipWith (*) s' . reverse . map fromIntegral
    where s' = 1:dropWhile (<=1) (convertI s)

  toBaseI :: Integral b => a -> b -> [Int]
  toBaseI s = scan' <*> (reverse . euclids)
    where --constructing place values
          euclids b         = takeWhile ((<=b) . abs) $ dropWhile ((<=1) . abs) $ convertI s
          scan' n []        = [fromIntegral n]                --allows explicit 0
          scan' n (x:xs)    = (fromIntegral num):scan' rem xs --generalized euclidean algorithm
            where (num,rem) = quotRem n x

seqcount s = map (toBaseI s) [0..]

--integer bases
instance IntegralBase Int where
  convertI x = iterate (*fromIntegral x) 1
instance IntegralBase Integer where
  convertI x = iterate (*fromIntegral x) 1

--integer sequences
instance IntegralBase [Int] where
  convertI = map fromIntegral
instance IntegralBase [Integer] where
  convertI = map fromIntegral

--convert a and b to (greedy) series representation
--multiply, then convert back
times :: (IntegralBase a, Integral b) => a -> b -> b -> b
times s a b = fromIntegral $ fromBaseI s badprod
  where badprod = fVect . product . map (prettify . toBaseI s) $ [a,b]

--convert a and b to (greedy) series representation, 
--multiply them, and subtract this from the proper product
deficient s a b = (a `times'` b) - a*b
  where times' = times s

--safe rounding for almost-integers
close :: (RealFrac a, Integral b) => a -> a -> Maybe b
close r x = do let x' = round x
               guard $ (<r) $ abs $ x - fromIntegral x'
               return x'

--nonintegral base typeclass
--Convert a floating-point number to/from an Fp ([Int] with a fractional sep)
--whose place values can be determined by `a`
class FractionalBase a where
  convertF  :: a -> Double
  {-# MINIMAL convertF #-}
  roundingF :: a -> Double
  roundingF _ = 1e-10

  --convert Fp to double using this interpretation
  fromBaseF :: a -> Fp -> Double
  fromBaseF a (Fp xs x) = sum $ zipWith (*) places $ map fromIntegral xs
    where r      = roundingF a
          b      = convertF a
          --need to bisect rounding value to test equality down to rounding
          places = takeWhile (>(r/2)) $ map (b^^) [x-1,x-2..]

  --convert Double to Fp naively
  toBaseF :: a -> Double -> Fp
  toBaseF a x | decimal > 0 = Fp (generate x 0) decimal
              | otherwise   = Fp (generate x (-decimal)) 0
    where r           = roundingF a
          b           = convertF a
          decimal     = 1 + (floor $ logBase b x)
          generate x h | x < r     = if h > 0 then replicate (decimal-h) 0 else []
                       | otherwise = sep ++ next:generate (rem*int) pow
            where pow         = floor $ logBase b x
                  int         = b^^pow
                  sep         = replicate (h-pow-1) 0
                  (next, rem) = properFraction $ x/int

--interpret an Fp as above, but try to find a 'close' integer
roundInterF :: (FractionalBase a, Integral b) => a -> Fp -> Maybe b
roundInterF a = close (roundingF a) . fromBaseF a

--simple base
instance FractionalBase Double where
  convertF = id

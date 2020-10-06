--module for extrapolating real/integral bases from linear recurrences.
--Fractional bases are those positive roots of the characteristic polynomial
--Integral bases are generalizations of zeckendorf expansions
module Math.Base.Recur where

import Prelude
import Control.Monad (guard)
import Math.Base.Base

--generate approximations for the root of the polynomial f (ascending powers)
--by newton's method
newton :: Fractional a => [a] -> a -> [a]
newton f = iterate (\x -> x - (eval f x) / (eval f' x))
  where f'   = zipWith (*) (tail f) $ map fromIntegral [1..]
        eval = flip $ \x -> foldr (\p acc -> p + x*acc) 0

--TODO this code is a little dirty and could probably be made more "functional"
--this is also only valid for the default `justify`, since stable reps are possible
--for recurrences like [1,0,1]
findbase (x:xs) = findbase' x xs
  where findbase' a []     = a --`Normal` contingency
        findbase' a (x:xs) | x <= -a   = stop
                           | x <= 0    = if negativecheck (a+x) xs then 0   else stop
                           | x <= a    = if lessthancheck x     xs then a+1 else stop
                           | otherwise = stop
        stop = error "Invalid recurrence"
        --digital root does not blow up
        negativecheck a []     = a > 0
        negativecheck a (x:xs) = x <= 0 && negativecheck (a+x) xs
        --monotonically decreasing
        lessthancheck a []     = True
        lessthancheck a (x:xs) = x > 0 && x <= a && lessthancheck x xs

--linear recurrence typeclass
class Recurrence a where
  recur :: a -> [Int]
  lead :: a -> Int
  lead _ = 1 --let's look at recurrences that involve integers (sans divisibility)
  --`Fp (recur a)` implies `Fp (1:recur *> pure 0)` (i.e., recur replaced with all 0's)
  {-# MINIMAL recur #-}

  --ceiling of `root a`, the number of digits allowed in the base
  --note that this is /not/ legal for countdorf countings, whose first digit is bounded
  --by the first term greater than 1
  base :: a -> Int
  base a = ceiling $ root 1e-1 a

  --efficient(-ish) place value justification
  --attempts to find a representation of a number without negatives
  justify :: a -> Fp -> Fp
  justify a b | seq base' True = recurse b
    where recur'     = recur a
          lead'      = lead a
          base'      = findbase recur'
          (o:os)     = fVect $ recurse $ Fp (base'-1:recur') 0; --fVect $ (count a) !! xx;
          recurlen   = length recur'
          recurse fp = justify' fp [] $ fVect fp
          justify' id _  []          = id --terminate check, return identity TODO: trim zeros
          justify' id zs xs@(xx:xxs)
            | base' > 0 && xx >= base' = match o zs $ zipAdd (0:xxs) os
            | otherwise                = case delta' of {
              Nothing -> justify' id (xx:zs) xxs; --continue trying
              Just as -> match lead' zs as;
    } where match b []     as = recurse $ Fp (b:as) $ 1 + fPlace id --nothing to rezip, add place value
            match b (z:zs) as = recurse $ Fp (foldl (flip (:)) (b+z:as) zs) $ fPlace id --rezip
            --if we run out of ys, then it's fine
            delta xs []         = Just xs
            --if the rest of the recurrence is negative, we append the negatives
            --otherwise, we give up trying to subtract this (this is handled later,
            --but the early Nothing helps terminate early)
            delta [] (y:ys)
                    | y <= 0    = (-y:) <$> delta [] ys
                    | otherwise = Nothing
            --subtract the place values
            delta (x:xs) (y:ys) = (x-y:) <$> delta xs ys
            --do the subtraction, returning only if all modfied digits are >= 0
            delta' = do a <- delta xs recur'
                        guard $ all (>=0) $ take recurlen a
                        return a

  --counting system induced by the root of the characteristic polynomial
  --of this recurrence relation, starting from 0
  count :: a -> [Fp]
  count a = iterate succ $ Fp [0] 1
    where succ y = justify a $ y + 1

--generalized recurrence sequence generator
linseq :: (Recurrence a, Integral b) => a -> [b]
linseq a = replicate initlen 0 ++ 1:rest
  where recur'    = reverse $ map fromIntegral $ recur a
        initlen   = (length recur') - 1
        rest      = zipadd $ map (flip drop $ linseq a) [0..initlen]
        zipadd xs = (sum $ zipWith (*) recur' $ map head xs):(zipadd $ map tail xs)

--limiting value of ratio between terms of `linseq`
--root :: (Recurrence a) => a -> Double -> Double 
root prec a = fst $ head $ dropWhile (\(x,y) -> abs (x-y) > prec) $ zip (tail xs) xs
  where recur' = map fromIntegral $ recur a
        lead'  = fromIntegral $ lead a
        xs     = flip newton cauchy $ reverse $ lead':map negate recur'
        cauchy = (1+) $ maximum $ map (/ lead') recur' --cauchy bound on roots

--n-nacci sequences; i.e. recurrences of the form a(m) = a(m-1) + a(m-2) + ...
data Nnacci = Nnacci Int
instance Recurrence Nnacci where
  recur (Nnacci n) = replicate n 1
instance IntegralBase Nnacci where
  convertI = linseq
instance FractionalBase Nnacci where
  convertF = (flip root) <*> roundingF

--not really lucas sequences, since the second term is negated in its recursion
--but I think that definition is stupid
data Lucas = Lucas Int Int
instance Recurrence Lucas where
  recur (Lucas a b) = [a,b]
instance IntegralBase Lucas where
  convertI = linseq
instance FractionalBase Lucas where
  convertF = (flip root) <*> roundingF

--companion pell numbers for Lucas sequences
pell :: Integral a => Lucas -> [a]
pell l@(Lucas a b) = 2:(fromIntegral a):rest
  where rest  = zipWith (+) as bs
        pell' = pell l
        as    = map ((fromIntegral a)*) $ tail pell'
        bs    = map ((fromIntegral b)*) pell'

--"normal" 1-term recurrence. simple geometric series
data Normal = Normal Int
instance Recurrence Normal where
  recur (Normal a) = [a]
instance IntegralBase Normal where
  convertI = linseq
instance FractionalBase Normal where
  convertF = (flip root) <*> roundingF

--degenerate 'literal' recurrence
--without additional constraints, `justify` CANNOT be guaranteed to work
data Literal = Literal { litName :: [Int] }
instance Recurrence Literal where
  recur = litName
instance IntegralBase Literal where
  convertI = linseq
instance FractionalBase Literal where
  convertF = (flip root) <*> roundingF

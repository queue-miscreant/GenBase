GenBase.Base
====================
Typeclasses implementing generalized counting system conversions.

Two systems are supported:
* Integral bases, where place values correspond to an integral sequence
* Fractional bases, where place values correspond to powers of a common ratio

`data Fp`
--------------------
```haskell
data Fp = Fp {fVect :: [Int], fPlace :: Int}
```
A generalized floating-point system; a list of `Int`s with decimal separator.
Without a standard carrying system, the "digits" (i.e. entries in `fVect`) can
grow as large as MAXINT.

### integ
```haskell
integ :: Fp -> [Int]
integ fp
```
Integral part of the `Fp`, before the fractional separator.


### integ
```haskell
frac :: Fp -> [Int]
frac fp
```
Fractional part of the `Fp`, after the fractional separator.


### prettify
```haskell
plVal :: Int -> Fp -> Int
plVal n fp
```
Extracts value at place value `n`, where the fractional separator is 0.


### prettify
```haskell
prettify :: Integral a => [a] -> Fp
prettify xs
```
Convert a list of integrals `xs` to an Fp, allowing use of the Num and Show instances

### Instances
#### `Num Fp`
`fp1` + `fp2` peforms addition where the fractional separators are aligned and
values out of range of either's list are 0.

`fp1` * `fp2` peforms multiplication by convolution, padding 0s as to the required
length and trimming to the nonzero portion. In "good" positional notations, this
the result is the representation of the product of the interpretations.


#### `Show Fp`
`show fp` displays the integer list with fractional separator, appearing like a
number in common positional notation.

The digit 0 is shown as '0', and all higher numbers are the successors of both sides.
e.g.: 5 = '5', 10 = ':'.
Digits with negative values are preceded by '-'.
Note that the sequence contained by an `Fp` can be infinite, so showTrunc should be used
to truncate.


#### `Read Fp`
`read fp` generally corresponds to the inverse of `show`.

The character 'r' is interpreted specially: characters after it are assumed to repeat
ad infinitum, as in an infinite decimal expansion.
```haskell
read ".r10" :: Fp
--the infinite expansion .1010101010...
```


`class IntegralBase`
--------------------
Integral bases, as mentioned above, are defined as those whose place values
correspond to a (monontonically increasing) integral sequence.

Formally, it is a standard conversion from a type into the sequence of the place values.
The minimal definition only includes convertI.

### convertI
```haskell
convertI :: (IntegralBase a) => a -> [b]
convertI base
```
Standard conversion to obtain the place values from the `base`.


### tobasei
```haskell
tobasei :: (IntegralBase a, Integral b) => a -> b -> [b]
tobasei base x
```
Converts the integral value `x` to a list of integrals in positional notation.
Note that the first index of the return value is the most significant digit.
Default implementation is greedy, based on the euclidean algorithm wrt place values.


### tobasei
```haskell
frombasei :: (IntegralBase a, Integral b) => a -> [b] -> b
frombasei base xs
```
Interprets the list `xs` as a number in the `base`'s positional notation.
The first index of `xs` is interpreted as the most significant digit.

### times
```haskell
times :: (IntegralBase a, Integral b) => a -> b -> b -> b
times base x y
```
Convert `x` and `y` to greedy representation in `base`, multiply by convolving,
and convert the result back.


### deficient
```haskell
times :: (IntegralBase a, Integral b) => a -> b -> b -> b
deficient base x y
```
Convert `x` and `y` to greedy representation in `base`, multiply by convolving,
then subtract the correct product out.


`class FractionalBase`
----------------------
Fractional bases are those whose place values are powers of some common ratio
(implemented as `Double`)

The minimal definition only includes convertF, the standard ratio.

### convertF
```haskell
convertF  :: FractionalBase a => a -> Double
```
Standard conversion to obtain the common ratio of the base.


### roundingF
```haskell
roundingF :: FractionalBase a => a -> Double
```
Standard rounding to be used for this instance. Defaults to `\_ -> 1e-10`


### tobasef
```haskell
tobasef :: a -> Double -> Fp
tobasef base x
```
Convert the value `x` to an `Fp` in this base, an integer list with decimal separator.


### frombasef
```haskell
frombasef :: a -> Fp -> Double
frombasef base fp
```
Interprets the values in `fp` as a number in the `base`'s positional notation.


### roundintf
```haskell
roundintf :: (FractionalBase a, Integral b) => a -> Fp -> Maybe b
roundintf base fp
```
`frombasef`, but attempts to convert the result into an integral value.

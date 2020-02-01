Ch_03.lhs

> module Ch_03 where
> import Test.QuickCheck

Chapter 3
Numbers

3.1 The type class Num
In Haskell, all numbers are instances of Num class:

  class (Eq a, Show a) => Num a where
    (+),(-),(*) :: a -> a -> a
    negate      :: a -> a
    abs, signum :: a -> a
    fromInteger :: Integer -> a

The minimal complete definition should be
  (+), (*), abs, signum, fromInteger, (negate | (-))
See
  http://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html#t:Num

3.2 Other numeric type classes
The Num class has 2 subclasses, the real numbers and the fractional numbers:

  class (Num a, Ord a) => Real a where
    toRational :: a -> Rational
  
  class (Num a) => Fractional a where
    (/)         :: a -> a -> a
    fromRationa :: Rational -> a

The type Rational is essentially a synonym for pairs of integers, the same as set theoretic implementation.

  *Ch_03> pi
  3.141592653589793
  *Ch_03> toRational pi
  884279719003555 % 281474976710656
  *Ch_03> 884279719003555 / 281474976710656
  3.141592653589793

One of the subclasses of the real numbers is the integral numbers.
A simplified version of this class is:

  class (Real a, Enum a) => Integral a where
    divMod    :: a -> a -> (a,a)
    toInteger :: a -> Integer

3.3 Computing floors

  *Ch_03> :t floor
  floor :: (RealFrac a, Integral b) => a -> b
  *Ch_03> floor 13
  13
  *Ch_03> floor 13.2
  13
  *Ch_03> floor 12.9
  12

> until' :: (a -> Bool) -> (a -> a) -> a -> a
> until' p f x = if p x 
>                  then x
>                  else until p f (f x)
  
  *Ch_03> until' (>100) (*7) 1
  343
  *Ch_03> 7^3
  343

> floor' x 
>   | x >= 0    = helper x 0
>   | otherwise = helper' x 0
>   where
>     helper a b
>       | a < fromInteger b = (b-1)
>       | otherwise = helper a (b+1)
>     helper' a b
>       | a >= fromInteger b = b
>       | otherwise = helper' a (b-1)

> prop_floor' x = floor x == floor' x
>   where
>     types = x :: Float
  
  *Ch_03> quickCheck prop_floor'
  +++ OK, passed 100 tests.

> floor'' x 
>   | x < 0     =  until (`leq` x) (subtract 1) (-1)
>   | otherwise = (until (x `lt`)  (+1)         1   ) -1 
>   where 
>     m `leq` x = fromInteger m <= x
>     x `lt`  m = x < fromInteger m

> prop_floor'' x = floor x == floor'' x
>   where
>     types = x :: Float

  *Ch_03> quickCheck prop_floor''
  +++ OK, passed 100 tests.

3.4 Natural numbers
Peano axiom

> data Nat = Zero | Succ Nat
>          deriving (Show, Ord, Eq)
>
> instance Num Nat where -- (+),(*),abs,signum,fromInteger,(negate | (-))
>   m + Zero   = m
>   m + Succ n = Succ (m+n)
>
>   m * Zero   = Zero
>   m * Succ n = m*n + m
>
>   abs n      = n
>
>   signum Zero     = Zero
>   signum (Succ n) = Succ Zero
>
>   fromInteger x
>     | x <= 0    = Zero
>     | otherwise = Succ (fromInteger (x-1))   
> 
>   m - Zero = m
>   Zero - _ = Zero -- (-) is total
>   (Succ m) - (Succ n) = m-n

Partial numbers
Since Succ is a non-strict function,
  Succ undefined
is an element of Nat, so called a partial number.

  *Ch_03> Zero == Succ undefined 
  False
  *Ch_03> Zero /= Succ undefined 
  True

In addition, there is another number:

  *Ch_03> let inf = Succ inf
  *Ch_03> :t inf
  inf :: Nat
  *Ch_03> Succ Zero == inf
  False

We can make Succ strict:

> data Nat' = Zero' | Succ' !Nat'

Exercise B

  *Ch_03> :t (^)
  (^) :: (Integral b, Num a) => a -> b -> a
  *Ch_03> :t (^^)
  (^^) :: (Fractional a, Integral b) => a -> b -> a
  *Ch_03> :t (**)
  (**) :: Floating a => a -> a -> a

  *Ch_03> 2 ^ 2
  4
  *Ch_03> 2.5 ^^ 2
  6.25
  *Ch_03> 2.5 ** 2.5
  9.882117688026186

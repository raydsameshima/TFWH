> module Ch_03 where

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



Ch03.lhs

> module Ch03 where
> import Test.QuickCheck ( quickCheck )
>
> import Prelude hiding ( divMod )

Chapter 3
Numbers

*Ch03> maxBound :: Int
9223372036854775807
*Ch03> logBase 2 $ fromIntegral (maxBound :: Int)
63.0

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

> dickFloor :: Float -> Integer
> dickFloor = read . takeWhile (/= '.') . show
>
> prop_dickFloor x = floor x == dickFloor x
>   where
>     types = x :: Float

  *Ch03> quickCheck prop_dickFloor 
  *** Failed! Falsifiable (after 2 tests):                  
  -2.1075156

So, we can modify his idea as follows:

> dickFloor' n
>   | n >= 0    = f
>   | otherwise = f - 1
>   where
>     f = dickFloor n
> prop_dickFloor' x = floor x == dickFloor' x
>   where
>     types = x :: Float

  *Ch03> quickCheck prop_dickFloor'
  +++ OK, passed 100 tests.
  (0.01 secs, 2,352,048 bytes)

> until' :: (a -> Bool) -> (a -> a) -> a -> a
> until' p f x = 
>   if p x 
>     then x
>     else until' p f (f x)
  
  *Ch_03> until' (>100) (*7) 1
  343
  *Ch_03> 7^3
  343

O(n) floor:

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
>
> prop_floor' x = floor x == floor' x
>   where
>     types = x :: Float

Appreciation of the need for conversion functions in some situations is one of 
the key points to understand about Haskell arithmetic.

> floor'' x 
>   | x < 0     =  until (`leq` x) (subtract 1) (-1)
>   | otherwise = (until (x `lt`)  (+1)         1   ) -1 
>
> m `leq` x = fromInteger m <= x
> x `lt`  m = x < fromInteger m
>
> prop_floor'' x = floor x == floor'' x
>   where
>     types = x :: Float

  *Ch03> quickCheck prop_floor'
  +++ OK, passed 100 tests.
  (0.03 secs, 12,719,240 bytes)
  *Ch03> quickCheck prop_floor''
  +++ OK, passed 100 tests.
  (0.02 secs, 2,986,104 bytes)

My definition takes more space, since it calls fromInteger more often.

Binary search
Shrinking interval [m, n) -> [m, m+1) is a better method.

O(log n) floor:

> floor''' :: Float -> Integer
> floor''' x = fst $ until unit (shrink x) (bound x)
>   where
>     unit :: Interval -> Bool
>     unit (m,n) = (m+1 == n)
>
> type Interval = (Integer, Integer)
>
> shrink :: Float -> Interval -> Interval
> shrink x (m,n) =
>   if p `leq` x
>     then (p,n)
>     else (m,p)
>     where
>       p = (m+n) `div` 2 
>
> bound :: Float -> Interval
> bound x =
>   let l = until (`leq` x) (*2) (-1)
>       u = until (x `lt`) (*2) 1
>   in (l,u)
>
> prop_floor''' x = floor x == floor''' x
>   where
>     types = x :: Float

  *Ch03> quickCheck prop_floor'''
  +++ OK, passed 100 tests.
  (0.01 secs, 2,041,408 bytes)

3.4 Natural numbers
Peano axiom

> data Nat = Zero | Succ Nat
>          deriving (Show, Eq)
>
> instance Num Nat where 
> -- (+),(*),abs,signum,fromInteger,(negate | (-))
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

  *Ch03> let ten = fromInteger 10
  (0.01 secs, 103,064 bytes)
  *Ch03> ten :: Nat
  Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))
  (0.01 secs, 169,040 bytes)

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
> --                        ^ strictnell flag                          

If we set such flag, our Nat' becomes just a set of finite numbers.

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

Exercise D
Even my one does not work properly:

  *Ch03> dickFloor' $ 1.23e7
  1
  (0.01 secs, 116,264 bytes)
  *Ch03> dickFloor $ 1.23e7
  1
  (0.00 secs, 111,112 bytes)

Exercise G
Let me use one of the minimal complete definitions: compare.

> instance Ord Nat where
>   Zero   `compare` Zero   = EQ
>   Zero   `compare` Succ _ = LT
>   Succ _ `compare` Zero   = GT
>   Succ a `compare` Succ b = a `compare` b
>
> divMod :: Nat -> Nat -> (Nat, Nat)
> divMod x y 
>   | x < y     = (Zero, x)
>   | otherwise = (Succ q, r)
>   where
>     (q,r) = divMod (x-y) y

  *Ch03> divMod 9 4
  (Succ (Succ Zero),Succ Zero)
  *Ch03> Prelude.divMod 9 4
  (2,1)


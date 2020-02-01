Ch01.lhs

> module Ch01 where
> import Test.QuickCheck
> import Data.List (sort)
> import Data.Ratio

Chapter 1
What is functional programming?

1.1 Functions and types
We usually write
  f(x)
for the function applycation, but, e.g.,
  sin t
rather than sin(t).

1.2 Functional composition
1.3 Example: common words
1.4 Example: numbers into words
1.5 The Haskell Platform
1.6 Exercises

Exercise A
  sum . map (2*) == (2*) . sum
  sum . map sum  == sum . concat
  sum . sort     == sum

From
  http://www.cs.nott.ac.uk/~pszgmh/fold.pdf
what we should start is to write these higher order function with foldr.

> sum' :: (Num n) => [n] -> n
> sum' = foldr (+) 0
> map' :: (a -> b) -> [a] -> [b]
> map' f = foldr ((:) . f) []
> append' :: [a] -> [a] -> [a]
> append' xs ys = foldr (:) ys xs
> concat' :: [[a]] -> [a]
> concat' = foldr append' []

and use the universality of foldr:
  g []     = v
  g (x:xs) = f x (g xs) <=> g = foldr f v

For 
  sum . map (2*) == (2*) . sum
case, consider the base cases.
  sum . map (2*) $ []
   == sum $ []
   == 0
and
  (2*) . sum $ []
   == (2*) 0
   == 0

  *Ch_01> sum . map (2*) $ []
  0
  *Ch_01> (2*) . sum $ []
  0

The induction cases are
  sum . map (2*) (x:xs)
   == sum ((2*x) : map (2*) xs)
   == 2*x + sum . map (2*) xs
   == (+) (2*x) sum . map (2*) xs
and
  (2*) . sum (x:xs)
   == (2*) (x + sum xs)
   == 2*x + (2*) . sum xs
   == (+) (2*x) (2*) . sum xs
Therefore, in each cases we have
  foldr ((+) . (2*)) 0

> prop_ExA xs = (sum . map (2*)) xs == ((2*) . sum) xs
>   where
>     types = xs :: [Integer]
> prop_ExA' xs = foldr ((+) . (2*)) 0 xs == ((2*) . sum) xs
>   where
>     types = xs :: [Rational]

  *Ch_01> quickCheck prop_ExA
  +++ OK, passed 100 tests.
  (0.02 secs, 6,259,784 bytes)
  *Ch_01> quickCheck prop_ExA'
  +++ OK, passed 100 tests.
  (0.81 secs, 423,715,464 bytes)

> prop_ExA'' xss = (sum . map sum) xss == (sum . concat) xss
>   where
>     types = xss :: [[Integer]]

  *Ch_01> quickCheck prop_ExA''
  +++ OK, passed 100 tests.
  (0.23 secs, 198,732,072 bytes)

> prop_ExA''' xs = (sum . sort) xs == sum xs
>   where
>     types = xs :: [Integer]

  *Ch_01> quickCheck prop_ExA'''
  +++ OK, passed 100 tests.

Exercise C
  () parehtneses
  [] brackets
  {} braces

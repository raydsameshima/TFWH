Ch_04.lhs

> module Ch_04 where

> import Data.List
> import Data.Char 

Chapter 4 Lists

4.1 List notation

The cons (:) operator is non-strict and returns a non strict function.
So we do know
  undefined : undefined
is non-empty!

  *Ch_04> null $ undefined : undefined 
  False

4.2 Enumerations
4.3 List comprehensions

> map' f xs = [f x | x<-xs]
> filter' p xs = [x | x<-xs, p x]
> concat' xss = [x | xs <- xss, x <- xs]

4.4 Some basic operations
4.5 Concatenation
append?

> append :: [a] -> [a] -> [a]
> []     `append` ys = ys
> (x:xs) `append` ys = x : (xs `append` ys)

4.6 concat, map and filter

> myConcat :: [[a]] -> [a]
> myConcat = foldr (++) []
>
> myMap :: (a -> b) -> [a] -> [b]
> myMap f  = foldr ((:) . f) []
>
> myFilter :: (a -> Bool) -> [a] -> [a]
> myFilter p = foldr (\x xs -> if p x 
>                                then x : myFilter p xs 
>                                else     myFilter p xs) []

Functor laws (of map)
  map id      = id
  map (g . f) = map g . map f

The second equation says that two traversals of a list can be replaced by one, with a corresponding gain in efficiency.

Naturality laws
  f . head       = head . map f
  map f . tail   = tail . map f
  map f . concat = concat . map (map f)

  map f . reverse = reverse . map f

  concat . map concat = concat . concat

  filter p . map f = map f . filter (p . f)

4.7 zip and zipWith
4.8 Common words, completed

Exercise E

> taxi3 n = [(a,d,b,c) | a<-[1..n],b<-[a+1..n],c<-[b+1..n],d<-[c+1..n], a^3+d^3==b^3+c^3]

  *Ch_04> map taxi3 [1..12]
  [[],[],[],[],[],[],[],[],[],[],[],[(1,12,9,10)]]

Exercise H

> myTake 0 xs = []
> myTake _ [] = []
> myTake n xx@(x:xs) 
>   | n > 0     = x : (myTake (n-1) xs)
>   | otherwise = xx
>

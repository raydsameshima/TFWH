Ch_02.lhs

> module Ch_02 where
>
> import Test.QuickCheck
> import Data.Char (isAlpha, toLower)

Chapter 2
Expressions, types and values

2.1 A session with GHCi

  *Ch_02> let f x y z = if x then y else z
  *Ch_02> :t f
  f :: Bool -> t -> t -> t

The bottom in Haskell is undefined.

  *Ch_02> :t undefined 
  undefined :: a
  *Ch_02> :i undefined 
  undefined ::
    forall (r :: GHC.Types.RuntimeRep) (a :: TYPE r).
    GHC.Stack.Types.HasCallStack =>
    a
      -- Defined in ‘GHC.Err’
  *Ch_02> undefined
  *** Exception: Prelude.undefined
  CallStack (from HasCallStack):
    error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
    undefined, called at <interactive>:37:1 in interactive:Ghci1

2.2 Names and operators
Sections and lambda expressions

2.3 Evaluation
Eager and Lazy evaluations.
  sqr x = x*x

  sqr (3+4)            sqr (3+4)
   == sqr 7             == let x = (3+4) in x*x
   == let x = 7 in x*x  == let x = 7 in x*x
   == 7*7               == 7*7
   == 49                == 49

The number of reduction steps is the same in each case.
With eager evaluation, arguments are always evaluated before a function is applied.
With lazy evaluation, the definition of a function is installed at once and only when they are needed are the arguments to the function evaluated.

  "call by need"

> infinity :: Integer
> infinity = 1+ infinity
> three _ = 3
  
  *Ch_02> :t three 
  three :: Num t => t1 -> t
  *Ch_02> three infinity 
  3
  *Ch_02> infinity 
  ^CInterrupted.

> factorial n = fact (n,1)
>   where
>     fact (x,y) = if x == 0 then y
>                            else fact (x-1,y*x) 

  *Ch_02> factorial 3
  6

Here is slightly different version of steps:
  factorial 3        factorial 3
   == fact (3,1)      == fact (3,1)
   == fact (3-1,3*1)  == fact (3-1,3*1)
   == fact (2,3*1)    == fact (2,3*1)
   == fact (2,3)      == fact (2-1,2*(3*1))
   == fact (2-1,2*3)  == fact (1,2*(3*1))
   == fact (1,2*3)    == fact (1-1,1*2*(3*1))
   == fact (1,6)      == fact (0,1*(2*(3*1)))
   == fact (1-1,1*6)  == 1*(2*(3*1))) 
   == fact (0,1*6)    == 6
   == fact (0,6) 
   == 6               
Lazy evaluation requires much more space to achieve the answer, the expression
  1*(2*(3*1))
is built up in memory before being evaluated.

Lazy evaluation terminates whenever ANY reduction order terminates; it never takes more steps than eager evaluation, and sometimes infinitely fewer.
However, it can require a lot more space and it is more difficult to understand the precise order in which things happen.

Strict and non-strict
A Haskell function f is said to be strict iff
  f undefined == undefined.
For example, three is non-strict, while (+) is strict in both arguments.

2.4 Types and type classes
Every Haskell type is a collection of right value and undefined.

2.5 Printing values

2.6 Modules

2.7 Haskell layout

2.8 Exercises

Exercise E

> first :: (a -> Bool) -> [a] -> Maybe a
> first _ [] = Nothing
> first p (x:xs) 
>   | p x       = Just x
>   | otherwise = first p xs

The number of functions of type
  Maybe a -> Maybe a

  Nothing -> Nothing
          -> undefined
  Just x  -> Nothing
          -> Just y
          -> undefined

Exercise H

> type CIN = String

> getDigit :: Char -> Int
> getDigit c = read [c]

  *Ch_02> map getDigit "63245134"
  [6,3,2,4,5,1,3,4]
  *Ch_02> sum it
  28

> addSum :: CIN -- 8 digits
>        -> CIN -- 10 digits (rest 2 digits are check sum)
> addSum cin = cin ++ (show d) ++ (show m)
>   where
>     (d,m) = divMod num 10
>     num = sum $ map getDigit cin

  *Ch_02> addSum "63245134"
  "6324513428"

> valid :: CIN -> Bool
> valid cin 
>   | length cin == 10 = addSum (take 8 cin) == cin
>   | otherwise        = False
  
  *Ch_02> valid "6324513428"
  True
  *Ch_02> valid "6324513426"
  False
  *Ch_02> valid "632451342838"
  False

Exercise I

> palindrome :: IO ()
> palindrome = do
>   putStrLn "Enter a string:"
>   str <- getLine
>   if isP str 
>     then putStrLn "Yes!"
>     else putStrLn "No!"
> -- Good to separate pure and impure (IO) parts of the functions.
> isP :: String -> Bool
> isP str = let str' = map toLower $ filter isAlpha str in
>   reverse str' == str' 

  *Ch_02 Data.Char> palindrome 
  Enter a string:
  Madam, I'm Adam
  Yes!
  *Ch_02 Data.Char> palindrome 
  Enter a string:
  A Man, a plan, a canal - Suez!
  No!
  *Ch_02 Data.Char> palindrome 
  Enter a string:
  Doc, note I dissent. A fast never prevents a fatness. I diet on cod.
  Yes!


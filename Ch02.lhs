Ch02.lhs

> module Ch02 where
>
> import Ch01 (commonWords)
>
> import Test.QuickCheck
> import Data.Char (isAlpha, toLower, digitToInt)
> import Data.List (reverse)

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
    (eager)              (lazy)
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

The pros and cons (its advantages and disadvantages)
Lazy evaluation terminates whenever ANY reduction order terminates; it never takes more steps than eager evaluation, and sometimes infinitely fewer.
However, it can require a lot more space and it is more difficult to understand the precise order in which things happen.

Strict and non-strict
A Haskell function f is said to be strict iff
  f undefined == undefined.
For example, three is non-strict, while (+) is strict in both arguments.

2.4 Types and type classes
Every Haskell type is a collection of right value and undefined.

2.5 Printing values
Printing the result involves the use of a function
  putStrLn :: String -> IO()
The type
  IO a
is a special type, in which we can employ IO computations that when executed have some interaction with outside world and return a value of type a.
When we do not need the returned value, as with putStrLn, we can use the null-tuple value
  () :: ().

To see an example, let us consider the follwoing program:

> cwords :: Int -> FilePath -> FilePath -> IO ()
> cwords n infile outfile = do
>   text <- readFile infile
>   writeFile outfile (commonWords n text)
>   putStrLn "cwords done!"

It reads the text from a file
   infile :: FilePath
and writes the output to a file
   outfile :: FilePath
where FilePath is a synomym for String.
(See also ex2_6.hs as another example.)

2.6 Modules

2.7 Haskell layout
offside rule

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


Luhn algorithm
https://www.rosettacode.org/wiki/Luhn_test_of_credit_card_numbers

*Ch02> map digitToInt $ show 49927398716
[4,9,9,2,7,3,9,8,7,1,6]

*Ch02> let ns = it
*Ch02> zip ns $ cycle [False, True]
[(4,False),(9,True),(9,False),(2,True),(7,False),(3,True),(9,False),(8,True),(7,False),(1,True),(6,False)]

*Ch02> ms = [x | (y, b) <- it, let x = if b then (2*y) else y]

*Ch02> ms
[4,18,9,4,7,6,9,16,7,2,6]

*Ch02> os = [x | y <- ms, let x = if y<10 then y else (1 + y `mod` 10)] 
*Ch02> os
[4,9,9,4,7,6,9,7,7,2,6]
*Ch02> sum it
70

> luhn :: Integer -> Bool
> luhn n = if n' `mod` 10 == 0
>            then True
>            else False
>   where 
>     n' = sum . preSum . double' . reverse . toDigits $ n
>
> toDigits :: Integer -> [Int]
> toDigits = map digitToInt . show
>
> double', preSum :: [Int] -> [Int]
>
> double' ds = 
>   [x | (y,b) <- zip ds (cycle [False, True])
>      , let x = if b then (2*y) else y]
>
> preSum ds = 
>   [x | y <- ds
>      , let x = if y < 10 then y else (1 + y `mod` 10)]

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


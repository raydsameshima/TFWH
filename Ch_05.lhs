Ch_05.lhs

> module Ch_05 where

Chapter 5
A simple Sudoku solver

5.1 Specification

> type Matrix a = [Row a]
> type Row a = [a]
>
> type Grid = Matrix Digit
> type Digit = Char
>
> digits :: [Char]
> digits = ['1' .. '9']
>
> blank :: Digit -> Bool
> blank = (== '0')

...to write down the simplest and clearest specification without regard to how efficient the result might be.
That's a key difference between functional programming and other forms of program construction: we can always begin with a clear and simple, though possibly extremely inefficient definition of solve, and then use the laws of functional programming to massage the computation into one that takes acceptable time and space.

Given grid, let us first complete it by filling in every possible choice for blank entries.
  completions :: Grid -> [Grid]
The result will be a list of filled grids.
Then we can filter this list for those do NOT contain duplicates in any row, box or column.
  valid :: Grid -> Bool

> solve :: Grid -> [Grid]
> solve = filter valid . completions

Let us work on completion first.

> completions :: Grid -> [Grid]
> completions = expand . choices

The function choices installs the available digits for each cell.

> choices :: Grid -> Matrix [Digit]
> choices = map (map choice) 

Grid is merely a synonym of Matrix Digit, and it is merely a list of lists.

> choice :: Digit -> [Char]
> choice d = if blank d -- d=='0'
>              then digits -- ['1' .. '9']
>              else [d]

After applying choices, we obtain a matrix each of whose entries is a list of digits.
What we want to do next is to define expand to convert this matrix (of lists) into a list of grids by installing all the choices in all possible ways.

Cartesian product

> cp :: [[a]] -> [[a]]
> cp [] = [[]]
> cp (xs:xss) = [x:ys | x<-xs, ys<-yss]
>   where
>     yss = cp xss
  
  *Ch_05> cp [[1],[2],[3]]
  [[1,2,3]]
  *Ch_05> cp [[1,2],[3],[4,5]]
  [[1,3,4],[1,3,5],[2,3,4],[2,3,5]]
  *Ch_05> cp [[1,2],[],[4,5]]
  []

What we should do is map cp function over each row, and apply cp function to the result to get all possible ways.

> expand :: Matrix [Digit] -> [Grid]
> expand = cp . map cp

Finally, we define the valid function.

> valid :: Grid -> Bool
> valid g = all noDups (rows g) &&
>           all noDups (cols g) &&
>           all noDups (boxs g)
>
> noDups :: (Eq a) => [a] -> Bool
> noDups []     = True
> noDups (x:xs) = all (/= x) xs && noDups xs
>
> -- primitive recursion
> noDups' :: (Eq a) => [a] -> Bool
> noDups' = fst . noDups''
>
> noDups'' :: (Eq a) => [a] -> (Bool, [a])
> noDups'' = foldr helper v
>   where
>     helper x (z,xs) = (all (/=x) xs && z, x:xs)
>     v = (True, [])
>
> rows :: Matrix a -> Matrix a
> rows = id
>
> cols :: Matrix a -> Matrix a
> cols [xs]     = [[x] | x <- xs]
> cols (xs:xss) = zipWith (:) xs (cols xss)
>
> boxs :: Matrix a -> Matrix a
> boxs = map ungroup . ungroup . map cols . (group 3) . map (group 3)
>
> group :: Int -> [a] -> [[a]]
> group _ [] = []
> group n xs = take n xs : group n (drop n xs)
>
> ungroup :: [[a]] -> [a]
> ungroup = concat

5.2 Lawful program construction 
Wholemeal programming.

Here we have three laws.
  rows . rows = id
  cols . cols = id
  boxs . boxs = id
The first two are valid on all matrices, and the third is valid on arbitrary n^2 * n^2 matrices.





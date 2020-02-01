Ch_05.lhs

> module Ch_05 where

Chapter 5
A simple Sudoku solver

5.1 Specification
Let us define our Matrix data type as a list of rows.
And a row is given by a list.

> type Matrix a = [Row a]
> type Row a = [a]

The sudoku matrix is now Grid.

> type Grid = Matrix Digit
> type Digit = Char
>
> aGrid :: Grid
> aGrid = 
>   [ "004005700"
>   , "000009400"
>   , "360000008"
>   , "720060000"
>   , "000402000"
>   , "000080093"
>   , "400000056"
>   , "005300000"
>   , "006100900"
>   ]
>
> digits :: [Char]
> digits = ['1' .. '9']
>
> blank :: Digit -> Bool
> blank = (== '0')

...to write down the simplest and clearest specification without regard to how efficient the result might be.
That's a key difference between functional programming and other forms of program construction:
we can always begin with a clear and simple, though possibly extremely inefficient definition of solve.
Then use the laws of functional programming to massage the computation into one that takes acceptable time and space.

A reasonable approach is to start with the given grid and to complete it by filling in every possible choice for the blank entries. 
The result will be a list of filled grid.
Then we can filter this list for those that do NOT contain duplicates in any raw, box or column.

> solve :: Grid -> [Grid]
> solve = filter valid . completions
 
Let us work on completions.

> completions :: Grid -> [Grid]
> completions = expand . choices
>
> choices :: Grid -> Matrix [Digit]
> choices = map (map choice)
> 
> choice :: Digit -> [Char]
> choice d
>   | blank d   = digits -- ['1' .. '9']
>   | otherwise = [d]
 
Cartesian product

> cp :: [[a]] -> [[a]]
> cp []       = [[]]
> cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
>   where
>     yss = cp xss -- cp xss is computed just once. 

This definition guarantees that cp xss is computed just once.
Here is an example.

  *Ch_05> cp [[1,2],[3,4,5]]
  [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5]]
  *Ch_05> cp [[1,2],[3],[4,5]]
  [[1,3,4],[1,3,5],[2,3,4],[2,3,5]]
  *Ch_05> cp [[1,2],[],[4,5]]
  []

What we shall do is first expand each rows, then expand it once more.

> expand :: Matrix [Digit] -> [Grid]
> expand = cp . map cp

> valid :: Grid -> Bool
> valid g = all noDups (rows g) &&
>           all noDups (cols g) &&
>           all noDups (boxs g)

Here we have used the following tutorial on foldr.
  http://www.cs.nott.ac.uk/~pszgmh/fold.pdf

> noDups :: (Eq a) => [a] -> Bool
> noDups = fst . noDups''
>
> noDups'' :: (Eq a) => [a] -> (Bool, [a])
> noDups'' = foldr helper (True, [])
>   where
>     helper x (z, xs) = (all (/=x) xs && z, x:xs)

> rows, cols, boxs :: Matrix a -> Matrix a
> rows = id
> -- transposing
> cols [xs]     = [[x] | x <- xs]
> cols (xs:xss) = zipWith (:) xs (cols xss)
> boxs = map ungroup . ungroup . map cols . group . map group
>
> group :: [a] -> [[a]]
> group = group' 3
>
> group' :: Int -> [a] -> [[a]]
> group' _ [] = []
> group' n xs = take n xs : group' n (drop n xs) 
> ungroup :: [[a]] -> [a]
> ungroup = concat

  *Ch_05> aGrid 
  ["004005700"
  ,"000009400"
  ,"360000008"
  ,"720060000"
  ,"000402000"
  ,"000080093"
  ,"400000056"
  ,"005300000"
  ,"006100900"
  ]
  *Ch_05> rows aGrid 
  ["004005700"
  ,"000009400"
  ,"360000008"
  ,"720060000"
  ,"000402000"
  ,"000080093"
  ,"400000056"
  ,"005300000"
  ,"006100900"
  ]
  *Ch_05> cols aGrid 
  ["003700400"
  ,"006200000"
  ,"400000056"
  ,"000040031"
  ,"000608000"
  ,"590020000"
  ,"740000009"
  ,"000009500"
  ,"008003600"
  ]
  *Ch_05> boxs aGrid 
  ["004000360"
  ,"005009000"
  ,"700400008"
  ,"720000000"
  ,"060402080"
  ,"000000093"
  ,"400005006"
  ,"000300100"
  ,"056000900"
  ]
  
We have already made every function that we need, but it takes too long.

5.2 Lawful program construction
  rows . rows = id -- easiest
  cols . cols = id -- difficult
  boxs . boxs = id -- need a lemma

Lemma
  ungroup . group = id
  group . ungroup = id -- valid only on grouped lists

Then
  boxs . boxs
   = (map ungroup . ungroup . map cols . group . map group)
     . (map ungroup . ungroup . map cols . group . map group)
   = map ungroup . ungroup . map cols . group . ungroup . map cols . group . map group
   = map ungroup . ungroup . map (cols . cols) . group . map group
If we assume
  cols . cols = id
then
  boxs . boxs = map ungroup . ungroup . group . map group
              = id

Here is three more laws, valid on n^2*n^2 matrices 
  map rows . expand = expand . rows
  map cols . expand = expand . cols
  map boxs . expand = expand . boxs

Finally, here are two laws about cp:
  map (map f) . cp    = cp . map (map f)
  filter (all p) . cp = cp . map (filter p)

5.3 Pruning the matrix of choices
Theoretically it's executable, but our definition of solve is hopeless in practice.
Assuming about 20 of 81 entries are given, there are about 
  *Ch_05> 9^61
  16173092699229880893718618465586445357583280647840659957609
grids to check!

Even worse, the minimum meaningful number of initial entries are 17:
  *Ch_05> 9^(81 -17)
  11790184577738583171520872861412518665678211592275841109096961
  *Ch_05> logBase 10 it
  61.07152060411679

To make a more efficient solver, an obvious idea is to remove any choices from a cell c that already occur as singleton entries in the row, column and box containing c.
This "singleton" means that the entry does not reappear in the row, column or box.

Since a matrix is a list of rows, let us start pruning a single row.

> pruneRow :: Row [Digit] -> Row [Digit]
> pruneRow row = map (remove fixed) row
>   where
>     fixed = [d | [d] <- row]
>
> remove :: [Digit] -> [Digit] -> [Digit]
> remove _  [x] = [x]
> remove ds xs  = filter (`notElem` ds) xs
  
  *Ch_05> pruneRow ["6", "12","3","134","56"]
  ["6","12","3","14","5"]
  *Ch_05> pruneRow ["6","36","3","134","4"]
  ["6","","3","1","4"]

The function pruneRow satisfies the following equation
  filter nodups . cp = filter nodups . cp . pruneRow
In words, this equation says that pruning a row will not throw away any list that contains no duplicates.








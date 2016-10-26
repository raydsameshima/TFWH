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


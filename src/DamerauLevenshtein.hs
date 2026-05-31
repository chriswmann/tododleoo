module DamerauLevenshtein (levenshtein) where

import Data.Array

levenshtein :: String -> String -> Int
levenshtein a b = table ! (m, n)
  where
    m = length a
    n = length b

    a' = listArray (1, m) a
    b' = listArray (1, n) b

    table :: Array (Int, Int) Int
    table =
      array
        ((0, 0), (m, n))
        [((i, j), cost i j) | i <- [0 .. m], j <- [0 .. n]]

    cost i j
      | i == 0 = j
      | j == 0 = i
      | otherwise = minimum (edits ++ transposition)
      where
        edits =
          [ table ! (i - 1, j) + 1, -- deletion
            table ! (i, j - 1) + 1, -- insertion
            table ! (i - 1, j - 1) + subsCost
          ]
        subsCost
          | a' ! i == b' ! j = 0
          | otherwise = 1
        transposition =
          [ table ! (i - 2, j - 2) + 1 -- transposition
          | i > 1,
            j > 1,
            a' ! (i - 1) == b' ! j,
            a' ! i == b' ! (j - 1)
          ]

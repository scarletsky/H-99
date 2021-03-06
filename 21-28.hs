{-# LANGUAGE FlexibleContexts #-}

import System.Random
import Data.List

-- 21 Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys i = take (i - 1) ys ++ x : drop (i - 1) ys

insertAt' x ys i = map snd as ++ x : map snd bs
    where ys' = zip [1..] ys
          f (j, _) = j < i
          (as, bs) = span f ys'


-- 22 Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range i j
    | i == j = [j]
    | otherwise = i : range (i+1) j

range' i j = [i..j]


-- 23 Extract a given number of randomly selected elements from a list.
rndSelect xs n = do
    gen <- getStdGen
    return $ map (xs !!) . take n $ randomRs (0, length xs - 1) gen


-- 24 Lotto: Draw N different random numbers from the set 1..M.
diffSelect n m = map f [0..n]
    where f x = head $ randomRs (0, m) (mkStdGen x)

diffSelect' n m = do
    gen <- getStdGen
    return $ take n $ nub (randomRs (0, m) gen)

diffSelect'' n m = rndSelect [0..m] n


-- 25 Generate a random permutation of the elements of a list.
rndPermu xs = do
    gen <- getStdGen
    let n = length xs
    return $ map (xs !!) $ take n $ nub (randomRs (0, length xs - 1) gen)


-- 26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
combinations _ [] = []
combinations 1 xs = map (\x -> [x]) xs
combinations n (x:xs) = combinations n xs ++ (map (x:) $ combinations (n-1) xs)


-- 27 Group the elements of a set into disjoint subsets.

combination :: Int -> [a] -> [([a], [a])]
combination 0 xs     = [([], xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
    where
        ts = [ (x:ys, zs) | (ys, zs) <- combination (n-1) xs ]
        ds = [ (ys, x:zs) | (ys, zs) <- combination n     xs ]

group' [] _ = [[]]
group' (n:ns) xs =
    [ g:gs | (g, rs) <- combination n xs
           , gs      <- group' ns rs ]

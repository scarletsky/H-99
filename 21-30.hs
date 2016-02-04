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


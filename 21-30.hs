-- 21 Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys i = take (i - 1) ys ++ x : drop (i - 1) ys

insertAt' x ys i = map snd as ++ x : map snd bs
    where ys' = zip [1..] ys
          f (j, _) = j < i
          (as, bs) = span f ys'


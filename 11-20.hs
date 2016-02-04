import Data.List

-- 11 (*) Modified run-length encoding.
data Code = Single Char
          | Multiple Int Char
          deriving (Show)

encodeModified = map f . group
    where f (all@(y:ys))
            | length all == 1 = Single y
            | otherwise       = Multiple (length all) y


-- 12 (**) Decode a run-length encoded list.
decodeModified [] = []
decodeModified ((Single x):xs) = x : decodeModified xs
decodeModified ((Multiple n x):xs) = replicate n x ++ decodeModified xs


-- 13 (**) Run-length encoding of a list (direct solution).
encodeDirect [] = []
encodeDirect (x:xs)
    | count == 1 = Single x : encodeDirect rest
    | otherwise  = Multiple count x : encodeDirect rest
    where (matched, rest) = span (== x) xs
          count = 1 + length matched


-- 14 (*) Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs

dupli' :: [a] -> [a]
dupli' = concatMap (\x -> x : [x])


-- 15 (**) Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] _     = []
repli (x:xs) n = replicate n x ++ repli xs n

repli' xs n = concatMap (\x -> replicate n x) xs


-- 16 (**) Drop every N'th element from a list.
dropEvery xs n = map snd $ filter f xs'
    where xs' = zip [1..] xs
          f (i, _) = i `mod` n /= 0

dropEvery' [] _  = []
dropEvery' xs n 
    | n <= 0     = xs
    | otherwise  = hs ++ dropEvery t n
    where (h, t) = splitAt n xs
          hs     = if (length h < n)
                   then h 
                   else init h 

dropEvery'' [] _ = []
dropEvery'' xs n = (init . take n) xs ++ dropEvery'' (drop n xs) n


-- 17 (*) Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split []     _   = ([], [])
split xs     0   = ([], xs)
split (x:xs) n   = (x : h, t)
    where (h, t) = split xs (n - 1)

split' xs n = (take n xs, drop n xs)

split'' = splitAt

-- 18 (**) Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice [] _ _        = []
slice xs i k        = map snd $ filter f xs'
    where xs'       = zip [1..] xs
          f (i', _) = (i' >= i && i' <= k)

slice' [] _ _ = []
slice' (all@(x:xs)) i k =  g all i k 1
    where g (y:ys) i k count
            | count < i = g ys i k (count + 1)
            | count > k = []
            | otherwise = y : g ys i k (count + 1)

slice'' xs i k = drop (i-1) $ take k xs


-- 19 (**) Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate [] _  = []
rotate xs n  = snd ys ++ fst ys
    where ys = split xs n'
          n' | n >= 0 = n
             | n < 0  = length xs - abs n

rotate' [] _     = []
rotate' xs 0     = xs
rotate' (x:xs) n
    | n < 0      = rotate' xs (length xs - abs n) ++ [x]
    | otherwise  = rotate' xs (n - 1) ++ [x]


-- 20 (*) Remove the K'th element from a list.
-- removeAt :: Int -> [a] -> ([a], [a])
removeAt _ []     = ([], [])
removeAt 1 (x:xs) = ([x], xs)
removeAt n (x:xs)
    | n < 1       = error "index should greater than 0"
    | otherwise   = (h, x:t)
    where (h, t)  = removeAt (n - 1) xs


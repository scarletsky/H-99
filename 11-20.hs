import Data.List

-- 11 (*) Modified run-length encoding.
data Code = Single Char
          | Multiple Int Char
          deriving (Show)

encodeModified = map (\(all@(y:ys)) -> if (length all == 1) then Single y else Multiple (length all) y) . group


-- 12 (**) Decode a run-length encoded list.
decodeModified [] = []
decodeModified ((Single x):xs) = x : decodeModified xs
decodeModified ((Multiple n x):xs) = replicate n x ++ decodeModified xs


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


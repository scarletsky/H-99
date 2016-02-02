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


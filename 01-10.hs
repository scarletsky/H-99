import Data.List

-- 1. (*) Find the last element of a list.

myLast1 :: [a] -> a
myLast1 []     = error "Empty list"
myLast1 (x:xs) = if (null xs) then x else myLast1 xs  

myLast2 :: [a] -> a
myLast2 []     = error "Empty list"
myLast2 [x]    = x
myLast2 (_:xs) = myLast2 xs

myLast3 :: [a] -> a
myLast3 []     = error "Empty list"
myLast3 (x:[]) = x
myList3 (_:xs) = myLast3 xs


-- 2. (*) Find the last but one element of a list.
myButLast :: [a] -> a
myButLast []       = error "Empty List"
myButLast (x:_:[]) = x 
myButLast (_:xs)   = myButLast xs


-- 3. (*) Find the K'th element of a list. The first element in the list is number 1.
elementAt [] _       = error "Empty list"
elementAt (x:_) 1    = x
elementAt all@(_:xs) n
    | n < 1          = error "Index should greater than 0"
    | n > length all = error "Index should less than list's length"
    | otherwise      = elementAt xs (n-1)


-- 4. (*) Find the number of elements of a list.
myLength :: (Num b) => [a] -> b
myLength []     = 0
myLength (_:xs) = 1 + myLength xs


-- 5. (*) Reverse a list.
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]


-- 6. (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs 


-- 7 (**) Flatten a nested list structure.
data NestedList a     = Elem a | List [NestedList a] deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)


-- 8 (**) Eliminate consecutive duplicates of list elements.
compress xs = foldl f [] xs
    where f acc x
            | x `elem` acc = acc
            | otherwise    = acc ++ [x]

-- 9 (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:ys) : pack zs
    where (ys, zs) = span ((==) x) xs

pack' xs = group xs

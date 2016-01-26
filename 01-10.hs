-- (*) Find the last element of a list.

myLast1 :: [a] -> a
myLast1 [] = error "Empty list"
myLast1 (x:xs) = if (null xs) then x else myLast1 xs  

myLast2 :: [a] -> a
myLast2 [] = error "Empty list"
myLast2 [x] = x
myLast2 (_:xs) = myLast2 xs

myLast3 :: [a] -> a
myLast3 [] = error "Empty list"
myLast3 (x:[]) = x
myList3 (_:xs) = myLast3 xs


-- (*) Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast (x:_:[]) = x 
myButLast (_:xs) = myButLast xs


-- (*) Find the K'th element of a list. The first element in the list is number 1.
elementAt [] _    = error "Empty list"
elementAt (x:_) 1 = x
elementAt all@(_:xs) n
    | n < 1          = error "Index should greater than 0"
    | n > length all = error "Index should less than list's length"
    | otherwise      = elementAt xs (n-1)

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


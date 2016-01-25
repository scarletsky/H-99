myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast (x:_:[]) = x 
myButLast (_:xs) = myButLast xs


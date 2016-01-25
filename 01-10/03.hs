elementAt [] _    = error "Empty list"
elementAt (x:_) 1 = x
elementAt all@(_:xs) n
    | n < 1          = error "Index should greater than 0"
    | n > length all = error "Index should less than list's length"
    | otherwise      = elementAt xs (n-1)


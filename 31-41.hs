-- 31 (**) Determine whether a given integer number is prime.
isPrime 2 = True
isPrime 3 = True
isPrime n = 0 `notElem` (map f [2..(n-1)])
    where f x = n `mod` x

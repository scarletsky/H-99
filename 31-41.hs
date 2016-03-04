-- 31 (**) Determine whether a given integer number is prime.
isPrime 2 = True
isPrime 3 = True
isPrime n = 0 `notElem` (map f [2..(n-1)])
    where f x = n `mod` x


-- 32 (**) Determine the greatest common divisor of two positive integer numbers.
myGCD x y = if (x `mod` y == 0) then y else myGCD y (x `mod` y)

myGCD' x 0 = abs x
myGCD' x y = myGCD' y (x `mod` y)

-- 33 (*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
coprime x y = if (myGCD' x y == 1) then True else False

coprime' x y = myGCD' x y == 1

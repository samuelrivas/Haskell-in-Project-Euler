-- List of prime numbers
primes = sieve [2..9999]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]
sieve [] = []

-- List of 4 digit prime numbers
fourDigitPrimes = [x | x <- primes, 1000 <= x]

-- Arithmetic sequences
arith_from x step (y:ys) =
    if (y - x) `mod` step == 0
    then
        y : arith_from x step ys
    else
        arith_from x step ys

arith_from _ _ _ = []



-- List of prime numbers
primes = sieve [2..9999]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]
sieve [] = []

-- List of 4 digit prime numbers
fourDigitPrimes = [x | x <- primes, 1000 <= x]

-- Arithmetic sequences
-- arith_seq (x:xs) = follow x xs : arith_seq xs
-- arith_seq [] = []

-- follow x (y:ys) = [x, y]
-- follow _ [] = []

-- Digits of a number
-- digits :: Int -> [Char]

-- digits n
--     | n < 10 = show n
--     | True = let d = rem n 10
--              in [(show d)]:(digits (div n 10))


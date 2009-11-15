-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
-- increases by 3330, is unusual in two ways: (i) each of the three terms are
-- prime, and, (ii) each of the 4-digit numbers are permutations of one another.
--
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
-- exhibiting this property, but there is one other 4-digit increasing sequence.
--
-- What 12-digit number do you form by concatenating the three terms in this
-- sequence?


-- List of prime numbers
primes = sieve [2..9999]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]
sieve [] = []

-- List of 4 digit prime numbers
fourDigitPrimes = [x | x <- primes, 1000 <= x]

-- Arithmetic sequences from the first element of the list
arith_seqs (x:y:t)
    | 2*y - x `elem` t = (x, y, 2*y - x) : arith_seqs (x:t)
    | True = arith_seqs (x:t)
arith_seqs _ = []


-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
-- increases by 3330, is unusual in two ways: (i) each of the three terms are
-- prime, and, (ii) each of the 4-digit numbers are permutations of one another.
--
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
-- exhibiting this property, but there is one other 4-digit increasing sequence.
--
-- What 12-digit number do you form by concatenating the three terms in this
-- sequence?

import List

-- List of prime numbers
primes = sieve [2..9999]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]
sieve [] = []

-- List of 4 digit prime numbers
fourDigitPrimes = [x | x <- primes, 1000 <= x]

-- Arithmetic sequences from the first element of the list
arith_seqs (x:y:t)
    | next `elem` t = (x, y, next) : arith_seqs (x:t)
    | True = arith_seqs (x:t)
    where next = 2*y - x

arith_seqs _ = []

-- Get the digits in base 10 of n
digits n = let aux
                   | n < 10 = [n]
                   | True = (n `mod` 10) : digits (n `div` 10)
           in List.sort aux

-- See whether a tuple has three numbers with the same digits
same_digits (x, y, z) =
    (digits x) == (digits y) && (digits x) == (digits z)

-- All arithmetic sequences filtered by same_digits
all_seqs l@(x:xs) = (filter same_digits (arith_seqs l)) ++ all_seqs xs
all_seqs [] = []

-- solution
get_sequence ((x,y,z):t) =
    if x == 1487 then
        get_sequence t
    else
        show x ++ show y ++ show z

solution = get_sequence $ all_seqs fourDigitPrimes
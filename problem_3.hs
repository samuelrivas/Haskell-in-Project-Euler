-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?

-- Infinite list of prime numbers
primes = sieve [2..]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]
sieve [] = []

factorisation 1 _ = []
factorisation n (x:primes) =
    if n `mod` x == 0
    then
        x : (factorisation (n `div` x) (x:primes))
    else
        factorisation n primes

result = last $ factorisation 600851475143 primes
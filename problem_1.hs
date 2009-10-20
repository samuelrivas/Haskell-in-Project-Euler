-- Find the sum of all the multiples of 3 or 5 below 1000.
sum_mults top = sum [x | x <- [0..top - 1], (rem x 3) * (rem x 5) == 0]

solution = sum_mults 1000
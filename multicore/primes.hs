-- http://de.scribd.com/doc/32353172/Multicore-Haskell-Now
-- ghc -O2 -fllvm --make primes.hs 
-- ghc -O2 --make -threaded primes.hs
-- ./primes +RTS -N8

main = print (take 10000 primes)

primes = sieve [2..]
    where 
        sieve (p:xs) =
            p : sieve [ x | x <- xs, x `mod` p > 0]


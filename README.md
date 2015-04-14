# haskell-seminar

## simple linear equation without parallel

working gauss haskell algorithmus


Install dependencies and build
```
cabal install
```

## folder other

* test (not working)

## Parallel Execution

```
ghc -O2 --make -threaded main-parallel.hs -rtsopts
time ./main-parallel +RTS -N2 -H4G -s -sstderr
```


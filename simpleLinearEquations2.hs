-- Base https://haskellicious.wordpress.com/2012/11/26/the-gauss-algorithm-in-haskell/

type Number = Double
type Vector = [Number]
type Row    = [Number]
type Matrix = [Row]

mapMatrix :: Matrix -> Vector -> Vector
mapMatrix matrix vector = [sum (zipWith (*) row vector) | row <- matrix]

gauss :: Matrix -> Vector -> Vector
gauss a b = x
    where
      b' = map (\y -> [y]) b
      a' = zipWith (++) a b' -- combine with right-hand side

      -- x = resubstitute $ triangular a'
      x = b 


triangular :: Matrix -> Matrix
triangular [] = []
triangular m = row:(triangular rows')
    where
      (row:rows) = rotatePivot m
      rows' = map f rows
      f bs
          | (head bs) == 0  = drop 1 bs
          | otherwise       = drop 1 $ zipWith (-) (map (*c) bs) row
          where
            c = (head row)/(head bs)

rotatePivot :: Matrix -> Matrix
rotatePivot (row:rows)
    | (head row) /= 0 = (row:rows)
    | otherwise       = rotatePivot (rows ++ [row])

exampleA = [[1,1,0], [0,1,1], [1,0,1]] :: Matrix
exampleb = [2,3,4] :: Vector

main = do
    print $ mapMatrix exampleA exampleb
    print $ triangular exampleA

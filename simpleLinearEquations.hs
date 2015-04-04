-- Base https://haskellicious.wordpress.com/2012/11/26/the-gauss-algorithm-in-haskell/

type Number = Double
type Vector = [Number]
type Row    = [Number]
type Matrix = [Row]

-- Führt eine Matrix Multiplikation mit einem Vector durch
-- Nicht direkt relevant für die Gauss Berechnung
mapMatrix :: Matrix -> Vector -> Vector
-- Durchläuft jede Reihe der Matrix, und multipliziert diese mit
-- dem Vector. Danach wird das Ergebniss zusammenaddiert auf einen Wert.
-- Die Ergebniss Reihen ergeben den zurückgegebenen Ergebnissvector
mapMatrix matrix vector = [sum (zipWith (*) row vector) | row <- matrix]

-- Kalkuliert Ergebniss Vector mit Gauss Elimination
-- First it consults the Matrix and Parameter Vecotor to
-- one Matrix
-- TODO remove comment if funciton complete
-- gauss :: Matrix -> Vector -> Vector
gauss a b = x
    where
        -- brings the vector in matrix format (double array form)
        b' = map (\y -> [y]) b
        -- appends/combine formated vector to matrix
        a' = zipWith (++) a b'
        
        -- x = resubstitute $ triangular a'
        -- brings matrix in triangle form
        x = triangular a'

-- Bringt die Matrix in die Dreiecksform, arbeitet rekursiv
-- Ruft als erstes rotatePivot auf die Matrix auf
triangular :: Matrix -> Matrix
-- Abbruch der rekusiven Funktion, wenn eine leere Menge übergeben wird da
-- letzte Reihe der Matrix erreicht wurde
triangular [] = []
-- Der Algorithmus arbeitet sich rekusiv, damit Reihe für Reihe durch die
-- Matrix 
triangular m = row:(triangular rows')
    where
        -- vertauscht ggf. erste reihe, solange bis keine 0
        (row:rows) = rotatePivot m
        -- führt die folgende Funktion f auf alle verbleibenden Reihen aus,
        -- rückgabwert ist das ergebniss
        -- Das Ergebniss besteht aus kleiner werdeden Reihen, also mit
        -- jedem Durchlauf, eine Reihe und eine Spalte weiter
        -- (Dreiecksform)
        rows' = map f rows
        -- bs ist die aktuelle Reihe
        -- Diese Funktion stellt sicher das alle Werte in der aktuell
        -- ersten Spalte auf 0 kommen, entweder sie sind es bereits oder
        -- durch Multiplikation der Reihen erreicht.
        -- Währendessen wird der erste Wert der Reihe (also die Spalte) 
        -- entfernt
        f bs
            -- wenn wert bereits 0 ist, dann entferne direkt ersten Wert
            -- aus der Reihe
            | (head bs) == 0  = drop 1 bs
            -- Multipliziere die Reihe mit der ersten Reihe (row) und dann
            -- entferne den ersten Wert
            | otherwise       = drop 1 $ zipWith (-) (map (*c) bs) row
            where
                -- Berechne Faktor zwischen erster Reihe und aktueller
                -- Reihe
                c = (head row)/(head bs)

-- Entfernt ggf. 0 Werte auf Multiplikationsstellen, arbeitet rekursiv 
-- Überprüft nur erste Reihe, wenn 0 ist, dann wird die Reihe unten
-- angehängt, und die Funtkion noch einmal aufgerufen, solange bis
-- in der ersten Reihe der Matrix kein 0 Wert mehr ist.
rotatePivot :: Matrix -> Matrix
rotatePivot (row:rows)
    -- not-equal operator
    | (head row) /= 0 = (row:rows)
    | otherwise       = rotatePivot (rows ++ [row])

-- Test 01
-- exampleA = [[1,1,0], [0,1,1], [1,0,1]] :: Matrix
-- exampleb = [2,3,4] :: Vector

-- Test 02
-- https://www.youtube.com/watch?v=2j5Ic2V7wq4
exampleA = [[1,1,-1], [0,1,3], [-1,0,-2]] :: Matrix
exampleb = [9,3,2] :: Vector

main = do
    print $ mapMatrix exampleA exampleb
    print $ gauss exampleA exampleb

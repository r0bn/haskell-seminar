-- Base https://haskellicious.wordpress.com/2012/11/26/the-gauss-algorithm-in-haskell/

import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock
import Data.Time

import Parser

type Number = Float 
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
gauss :: Matrix -> Vector -> Vector
gauss a b = x
    where
        -- brings the vector in matrix format (double array form)
        b' = map (\y -> [y]) b
        -- appends/combine formated vector to matrix
        a' = zipWith (++) a b'
        
        -- brings matrix in triangle form and resubstitute
        x = resubstitute $ triangular a'

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
        rows' = parMap (rdeepseq) f rows
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


-- First ververse the columns to have the last unknown in the head column
-- and reverse the rows as well to start with the equation that has only on
-- unkown. Afterward wie to undo the column reversion
resubstitute :: Matrix -> Vector
resubstitute = reverse . resubstitute' . reverse . map reverse

-- calculate the solution from the first equation with only one unknown and
-- substitute this result into the other equation
resubstitute' :: Matrix -> Vector
resubstitute' [] = []
-- works rekursive, every round elimantes one variable
resubstitute' (row:rows) = x:(resubstitute' rows')
    where
        -- calculate current variable
        x       = (head row)/(last row)
        -- insert in every other pending equation
        rows'   = map substituteUnknown rows
        -- calcualte and consult for the next round
        substituteUnknown (a1:(a2:as')) = ((a1 - x * a2):as')

-- Test 01
-- exampleA = [[1,1,0], [0,1,1], [1,0,1]] :: Matrix
-- exampleb = [2,3,4] :: Vector

-- Test 02
-- https://www.youtube.com/watch?v=2j5Ic2V7wq4
exampleA = [[1,1,-1], [0,1,3], [-1,0,-2]] :: Matrix
exampleb = [9,3,2] :: Vector

-- Test 03
-- moodle x3 is free, creates infinite loop at rotatePivot
-- exampleA = [[1,2,3], [4,5,6], [7,8,9]] :: Matrix
-- exampleb = [1,2,3] :: Vector

main = do
    x <- loadMatrix "../MATRIX Seminar/matrix_1024x1024.csv"

    let cols = getCols x
    let rows = getRows x
    --print cols
    --print rows
    let matrix = getMatrix x cols rows
    let vector = getVector x cols
--    print matrix
--    print vector

    start <- getCurrentTime
    let! ergebnis = gauss matrix vector
    end <- getCurrentTime
--    fprint (timeSpecs % "\n") start end
    print ergebnis
    print $ diffUTCTime end start
    print start
    print end

--    print $ gauss matrix vector


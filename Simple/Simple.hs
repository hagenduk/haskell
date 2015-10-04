module Simple
where

-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 
fib     :: Integer -> Integer
fib x = undefined


-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 mit linearer Laufzeit

fib2    :: Integer -> Integer
fib2 = undefined

-- Definieren Sie eine Funktion c (für Collatz), die berechnet
-- wie viele Rekursionsschritte benötigt werden, um
-- eine natürliche Zahl n >= 1 auf 1 zu
-- reduzieren.
--
-- Folgende Reduktionsregel sind dabei anzuwenden: Wenn n gerade ist,
-- so wird n halbiert, wenn n ungerade ist, so wird n verdreifacht und um
-- 1 erhöht.
    
c       :: Integer -> Integer
c n = undefined


-- Definieren Sie ein endrekurive Variante von c
    
c1      :: Integer -> Integer
c1 = undefined


-- Definieren Sie eine Funktion cmax, die für ein
-- Intervall von Zahlen das Maximum der
-- Collatz-Funktion berechnet. Nutzen Sie die
-- vordefinierten Funkt min und max.

cmax    :: Integer -> Integer -> Integer
cmax lb ub = undefined


-- Definieren Sie eine Funktion imax, die für ein
-- Intervall von Zahlen das Maximum einer
-- ganzzahligen Funktion berechnet. Formulieren
-- Sie die obige Funktion cmax so um, dass sie mit imax arbeitet.

imax    :: (Integer -> Integer) -> Integer -> Integer -> Integer
imax f lb ub = undefined


cmax1   :: Integer -> Integer -> Integer
cmax1
    = imax c1

-- Entwickeln Sie eine Funktion,
-- die die Position und den Wert bestimmt, an der
-- das Maximum angenommen wird.
-- Versuchen Sie, eine endrekursive Lösung zu finden
-- (mit einer lokalen Hilfsfunktion).

imax2   :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer)
imax2 f lb ub = undefined

-- ----------------------------------------

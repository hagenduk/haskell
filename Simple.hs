module Simple
where

-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 
fib     :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = (fib (n-1)) + (fib (n-2))


-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 mit linearer Laufzeit

fib2    :: Integer -> Integer
fib2 n = fib2' n 0 1
fib2' n a b 
	| n == 0 = 0
	| n == 1 = 1
	| n == 2 = (a+b)
	| otherwise = fib2' (n-1) b (a+b)  

-- Definieren Sie eine Funktion c (für Collatz), die berechnet
-- wie viele Rekursionsschritte benötigt werden, um
-- eine natürliche Zahl n >= 1 auf 1 zu
-- reduzieren.
--
-- Folgende Reduktionsregel sind dabei anzuwenden: Wenn n gerade ist,
-- so wird n halbiert, wenn n ungerade ist, so wird n verdreifacht und um
-- 1 erhöht.
c1      :: Integer -> Integer
c1 n = c1' n
	where
	c1' n
		| n <= 0 = error "<=0"
		| n == 1 = 0
		| even n = 1 + c1' (div n 2)
		| otherwise = 1+ c1' ((n* 3) +1)



-- Definieren Sie ein endrekurive Variante von c
    
c       :: Integer -> Integer
c n = c' n 0
	where
	c' n x
		| n <= 0 = error "<=0"
		| n == 1 = x
		| even n = c' (div n 2) (x+1)
		| otherwise = c' ((n * 3)+1) (x+1) 


-- Definieren Sie eine Funktion cmax, die für ein
-- Intervall von Zahlen das Maximum der
-- Collatz-Funktion berechnet. Nutzen Sie die
-- vordefinierten Funkt min und max.

cmax    :: Integer -> Integer -> Integer
cmax lb ub
	| lb > ub = error "falsche werte"
	| lb == ub = (c lb)
	| lb < ub = max (c lb) (cmax (lb +1) ub)


-- Definieren Sie eine Funktion imax, die für ein
-- Intervall von Zahlen das Maximum einer
-- ganzzahligen Funktion berechnet. Formulieren
-- Sie die obige Funktion cmax so um, dass sie mit imax arbeitet.

imax    :: (Integer -> Integer) -> Integer -> Integer -> Integer
imax f lb ub
	| lb > ub = error "falsche werte"
	| lb == ub = (f lb)
	| lb < ub = max (f lb) (cmax (lb +1) ub)


cmax1   :: Integer -> Integer -> Integer
cmax1
    = imax c1

-- Entwickeln Sie eine Funktion,
-- die die Position und den Wert bestimmt, an der
-- das Maximum angenommen wird.
-- Versuchen Sie, eine endrekursive Lösung zu finden
-- (mit einer lokalen Hilfsfunktion).

imax2   :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer)
imax2 f lb ub = imax2' f lb ub (0,0)
		where
		imax2' f lb ub (a,b)
			| lb > ub = error "falsche werte"
			| lb == ub = max ((f lb), lb) (a,b)
			| lb < ub = imax2' f (lb+1) ub (max (a,b) ((f lb),lb))

-- ----------------------------------------

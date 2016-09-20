module Simple
where

-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 
fib     :: Integer -> Integer
fib x  
    | x == 1 = 1
    | x > 1  = fib(x-1) + fib(x-2)


-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 mit linearer Laufzeit

fib2    :: Integer -> Integer

fib2 =
   fib2' 0 1
   where
   fib2' x0 x1 0 = x0
   fib2' x0 x1 n = fib2' x1 (x0 + x1) (n-1)


-- Definieren Sie eine Funktion c (für Collatz), die berechnet
-- wie viele Rekursionsschritte benötigt werden, um
-- eine natürliche Zahl n >= 1 auf 1 zu
-- reduzieren.
--
-- Folgende Reduktionsregel sind dabei anzuwenden: Wenn n gerade ist,
-- so wird n halbiert, wenn n ungerade ist, so wird n verdreifacht und um
-- 1 erhöht.
    
c       :: Integer -> Integer
c n
    | n == 1 = 0
    | even(n) = 1 + c (n `div` 2)
    | odd(n) = 1 + c (n * 3 + 1)


-- Definieren Sie ein endrekurive Variante von c
    
c1      :: Integer -> Integer

c1 = c1' 0
    where
    c1' r n 
        | n == 1  = r
        | even(n) = c1' (r+1) (n `div` 2)
        | odd(n)  = c1' (r+1) (n * 3 + 1)



-- Definieren Sie eine Funktion cmax, die für ein
-- Intervall von Zahlen das Maximum der
-- Collatz-Funktion berechnet. Nutzen Sie die
-- vordefinierten Funkt min und max.

cmax    :: Integer -> Integer -> Integer
cmax'    :: Integer -> Integer -> Integer

cmax' lb ub = cmaxIter lb (c lb)
  where cmaxIter i maximum
          | i == ub = maximum
          | i < ub  = cmaxIter (i + 1) (max maximum $ c (i + 1))
          | i > ub  = error "lower bound is bigger than upper"

cmax lb ub 
    | lb == ub = c lb
    | otherwise = max (c lb)  (cmax (lb + 1) ub) 




-- Definieren Sie eine Funktion imax, die für ein
-- Intervall von Zahlen das Maximum einer
-- ganzzahligen Funktion berechnet. Formulieren
-- Sie die obige Funktion cmax so um, dass sie mit imax arbeitet.

imax    :: (Integer -> Integer) -> Integer -> Integer -> Integer
imax f lb ub = imaxIter lb (f lb)
  where imaxIter i maximum
          | i == ub = maximum
          | i < ub  = imaxIter (i + 1) (max maximum ( f (i + 1)))
          | i > ub  = error "lower bound is bigger than upper"


cmax1   :: Integer -> Integer -> Integer
cmax1
    = imax c1

-- Entwickeln Sie eine Funktion,
-- die die Position und den Wert bestimmt, an der
-- das Maximum angenommen wird.
-- Versuchen Sie, eine endrekursive Lösung zu finden
-- (mit einer lokalen Hilfsfunktion).

imax2   :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer)
imax2 f lb ub = imax2Iter lb (lb, (f lb))
  where imax2Iter i (maxi, val)
          | i == ub = (maxi, val)
          | i < ub  = let m = f i 
                      in imax2Iter (i + 1) (if m > val then i else maxi, max m val)
          | i > ub  = error "lower bound is greater than upper"



-- ----------------------------------------

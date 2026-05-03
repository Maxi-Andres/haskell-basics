-- EJERCICIO 1: APLICACIÓN PARCIAL (TIPADO)
-- Determinar los tipos resultantes de las siguientes aplicaciones parciales:
-- a) :t (== 'a')
-- b) :t (+5)
-- c) :t max 0

-- EJERCICIO 2: DEFINICIÓN -- ! POINT-FREE
-- Definir las siguientes funciones utilizando el estilo Point-Free (eliminando el parámetro explícito):
-- a) esMayor: Edad -> Bool (usando >= 18)
-- b) doble: Num a => a -> a (usando 2 *)
-- c) alMenosCero: (Num a, Ord a) => a -> a (usando max 0)

esMayorDeEdad :: (Ord a, Num a) => a -> Bool
esMayorDeEdad = (>= 18)

doble :: (Num a) => a -> a
doble = (2 *)

alMenosCero :: (Num a, Ord a) => a -> a
alMenosCero = (max 0)

-- EJERCICIO 3: COMPOSICIÓN DE FUNCIONES
-- Dada la función:
-- promociona alumno = not (esMenorAOcho (nota alumno))
-- Redefinirla utilizando el operador de composición (.).

data Alumno = Alumno
  { calificacion :: Int,
    nombre ::
      String
  }
  deriving (Show)

carlos :: Alumno
carlos = Alumno 4 "carlos"

nerd :: Alumno
nerd = Alumno 9 "carlos"

nota :: Alumno -> Int
nota al = calificacion al

esMenorAOcho :: Int -> Bool
esMenorAOcho = (< 8)

promociona :: Alumno -> Bool
-- promociona alumno = not (esMenorAOcho (nota alumno))
promociona = not . esMenorAOcho . nota

-- EJERCICIO 4: RECURSIVIDAD Y CAPICÚA
-- Completar la definición de la función 'esCapicua' utilizando las funciones 'head', 'last' e 'init':
-- esCapicua [] = True
-- esCapicua [_] = True
-- esCapicua ...

esCapicua :: (Eq a) => [a] -> Bool
esCapicua [] = True
esCapicua [_] = True
esCapicua [x, y] = x == y
esCapicua [x, _, y] = x == y
esCapicua (x : xs) = x == last xs && esCapicua (init xs) -- ? init te da toda la lista sin el ultimo

-- * (x : xs) esto hace [1,2,3] => [1] [2,3]

-- EJERCICIO 5: TRANSFORMAR A BINARI
-- Genere la función a la cual se le pasa un numero decimal y devuelve uno binario

decimalABinario :: Int -> Int
decimalABinario 0 = 0
decimalABinario 1 = 1
decimalABinario n = mod n 2 + decimalABinario (div n 2) * 10

-- EJERCICIO 6: RECURSIVIDAD SOBRE ESTRUCTURAS (SUMATORIA)
-- Genere la función 'sumatoria' que sume todos los elementos de una lista de números.

sumatoria :: (Num a) => [a] -> a
sumatoria [] = 0
sumatoria (x : xs) = x + sumatoria xs

-- EJERCICIO 7: RECURSIVIDAD SOBRE ESTRUCTURAS (LONGITUD)
-- Genere la función 'longitud' de una lista sin utilizar la función 'length' predefinida.

longitud :: (Num a) => [a] -> a
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs -- ! si definis (x : xs) en vez de (_ : xs) se enoja si no usas x

-- EJERCICIO 8: RECURSIVIDAD NUMÉRICA (POTENCIA)
-- Genere una función para calcular un número elevado a otro número (potencia) de forma recursiva.

(##) :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2 -- ! como tiene ( ) le avisas al compilador que es un operador infijo
(##) _ 0 = 1
(##) num pot = num * (num ## (pot - 1))

-- EJERCICIO 9: RECURSIVIDAD CON LÍMITE (TOPE)
-- Genere una función que, mientras no se supere un valor tope, muestre los elementos de la lista.

mostrarElementosAntesDeValorTope :: (Ord a) => [a] -> a -> [a]
mostrarElementosAntesDeValorTope [] _ = [] -- Caso base: lista vacía
mostrarElementosAntesDeValorTope (x : xs) tope
  | x < tope = x : mostrarElementosAntesDeValorTope xs tope -- Si es menor, lo incluyo y sigo
  | otherwise = [] -- Si llegué al tope, corto la lista (devuelvo vacío)

-- EJERCICIO 10: RECURSIVIDAD (TAKE)
-- Implementar manualmente la función 'take' que devuelva los primeros 'n' elementos de una lista.

takeManual :: Int -> [a] -> [a]
takeManual n _ | n <= 0 = [] -- Caso base 1: si n es 0 o negativo, no quiero nada
takeManual _ [] = [] -- Caso base 2: si la lista se vacía, no hay nada que sacar
takeManual n (x : xs) = x : takeManual (n - 1) xs -- Regla recursiva
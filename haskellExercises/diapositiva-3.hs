-- EJERCICIO 1
-- Definir la función factorial utilizando Pattern Matching. Establecer el caso base para 0 y la regla recursiva para n. Reflexionar: ¿qué sucede si se intenta calcular el factorial de -1?

calcularFactorial :: Int -> Int
calcularFactorial 0 = 1
calcularFactorial n = calcularFactorial (n - 1) * n

-- EJERCICIO 2
-- Definir la función enesimocaracter que reciba un número n y una palabra, y devuelva el carácter en esa posición (utilizando el operador !!). Realizar la firma de tipo explícita (Int -> String -> Char) y verificar qué error arroja el sistema si se intenta pasar un Bool en lugar del Int.

enesimoCaracter :: Int -> String -> Char
enesimoCaracter n texto = texto !! n

-- EJERCICIO 3 (DESAFÍO DE INFERENCIA)
-- Determinar cuál es la firma de tipo que el motor de Haskell infiere para la función: ff x y = x && not y. Explicar el razonamiento analizando los operadores involucrados.

ff x y = x && not y

-- EJERCICIO 4 (VARIABLES DE TIPO)
-- Definir las funciones identidad (id x = x) e ignorarElPrimero (ignorar x y = y). Determinar sus firmas de tipo utilizando variables de tipo (a, b) y explicar por qué no se utilizan tipos concretos como Int o Bool.

-- id :: a -> a   (funciona para cualquier tipo)
id x = x

-- ignorarelprimero :: a -> b -> b
ignorarelprimero x y = y

-- EJERCICIO 5 (TYPE CLASSES)
-- Dada la función doble x = 2 * x, analizar por qué su firma es Num a => a -> a. Verificar qué sucede al ejecutar la función con un Float y con un Char, y explicar el error "No instance for (Num Bool)" en caso de usar un booleano.

doble x = 2 * x

-- EJERCICIO 6
-- Implementar la función elMayorDeLosTres que reciba tres valores y devuelva el mayor. Asegurar que la firma de tipo utilice la restricción Ord para permitir la comparación.

-- * lo que haces decirle al compilador que a tiene un tipo definido

elMayorDeLosTres :: (Ord a) => a -> a -> a -> a
elMayorDeLosTres x y z = max x (max y z)

-- EJERCICIO 7
-- Definir la función Xor (OR exclusivo) que reciba dos valores booleanos y devuelva True solo si uno de los dos es verdadero.

compuertaXor :: Bool -> Bool -> Bool
compuertaXor False False = False
compuertaXor False True = True
compuertaXor True False = True
compuertaXor True True = False

-- EJERCICIO 8
-- Implementar la secuencia de Fibonacci en Haskell utilizando Pattern Matching y recursividad.

secuenciaFibonacci :: Int -> Int
secuenciaFibonacci 0 = 0
secuenciaFibonacci 1 = 1
secuenciaFibonacci n = secuenciaFibonacci (n - 1) + secuenciaFibonacci (n - 2)

-- EJERCICIO 9 (INFERENCIA AVANZADA)
-- Determinar el tipo de las siguientes funciones basándose en su lógica:
-- 1. esMuchoMayor n m = n - m > 10
-- 2. funcionRara n m = esMuchoMayor n (not m)
-- 3. funcionRara1 f = f 2 True
-- 4. funcionRara2 g = g . length

-- EJERCICIO 10 (ALIAS DE TIPOS)
-- Crear alias de tipo (type) para Nombre y Edad. Definir una función edad que reciba un nombre y devuelva la edad de personas específicas (ej. Mati: 23, Leo: 26) usando Pattern Matching, aplicando los alias en la firma de la función.

-- ! type es simplemente un alias le das un "apodo" a un tipo de dato

type Nombre = String

type Edad = Int

edad :: Nombre -> Edad
edad "Mati" = 23
edad "Leo" = 26
edad _ = 0
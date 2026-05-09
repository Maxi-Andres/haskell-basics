-- 1. Múltiplos:
-- Definir funciones para determinar si un número es múltiplo de dos, tres y diez.
-- Luego, generalizar en una función 'esMultiploDe' que reciba el divisor y el número.

esMultiploDeDos :: Int -> Bool
esMultiploDeDos x = mod x 2 == 0

esMultiploDeTres :: Int -> Bool
esMultiploDeTres x = mod x 3 == 0

-- ? a esto se le llama funcion de orden superior

-- * No exactamente. Una función es de Orden Superior si recibe otra función como argumento (como map) o si devuelve una función. esMultiploDe solo recibe dos números.

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

-- 2. Ordenamiento:
-- Implementar funciones para ordenar una lista alfabéticamente, por precio o por calorías.
-- Abstraer la lógica usando Orden Superior con una función 'ordenarPor'.

-- 3. Promedio de Edades:
-- Realizar el promedio de edad de una lista de personas.
-- Nota: Usar 'sum' para la suma de edades y dividir por el 'length' de la lista.

data Persona = Persona
  { nombre :: String,
    edad :: Float
  }
  deriving (Show)

alice :: Persona
alice = Persona "alice" 5

pepe :: Persona
pepe = Persona "pepe" 51

mati :: Persona
mati = Persona "mat" 80

promedioDeEdad :: [Persona] -> Float
promedioDeEdad persons = sum (map edad persons) / fromIntegral (length persons)

-- 4. Doble de una lista:
-- Crear la función 'duplicarTodos' que calcule el doble de cada número en una lista.

duplicarLista :: (Num a) => [a] -> [a]
duplicarLista [] = []
duplicarLista (x : xs) = x * 2 : duplicarLista xs

sumarTodos :: (Num a) => [a] -> a
sumarTodos list = sum list

-- 5. Iniciales:
-- Obtener las iniciales de una lista de palabras (función 'iniciales').

listaInicialesFacil :: [String]
listaInicialesFacil = ["hola", "como", "estas"]

obtenerIniciales :: [String] -> [Char]
obtenerIniciales [] = []
obtenerIniciales (x : xs) = head x : obtenerIniciales xs

-- En Haskell, el símbolo : se conoce como el constructor "cons" (de construct)

-- * (x : xs) esto hace [1,2,3] => [1] [2,3]

-- ! : esto sirve para ir creando la lista EJ: si pones ["Hola", "Mundo"] te da 'H' : ('M' : obtenerIniciales []) y te devuelve "HM"

-- 6. Implementación de Map (Recursividad):
-- Implementar manualmente la función 'map' usando patrones de lista (vacia y cabeza:cola).

miMap :: (a -> b) -> [a] -> [b]
miMap _ [] = []
miMap f (x : xs) = f x : miMap f xs

-- 7. Refactorización con Map:
-- Reescribir las funciones 'edades', 'duplicarTodos' e 'iniciales' utilizando la función 'map'
-- y luego pasar a una definición con estilo tácito (point-free).

-- Versión normal: edades personas = map edad personas
edades :: [Persona] -> [Float]
edades = map edad

-- Versión normal: duplicarTodos xs = map (*2) xs
duplicarTodos :: (Num a) => [a] -> [a]
duplicarTodos = map (* 2)

-- Versión normal: iniciales xs = map head xs
iniciales :: [String] -> [Char]
iniciales = map head

-- 8. Recursividad en Funciones Estándar:
-- Implementar manualmente las funciones: head, tail, null, elem, TodosPares y TodosAprobados.

-- 1. head: Devuelve el primer elemento
miHead :: [a] -> a
miHead [] = error "Lista vacía"
miHead (x : _) = x

-- 2. tail: Devuelve todo menos el primero
miTail :: [a] -> [a]
miTail [] = error "Lista vacía"
miTail (_ : xs) = xs

-- 3. null: ¿Está vacía?
miNull :: [a] -> Bool
miNull [] = True
miNull _ = False

-- 4. elem: ¿El elemento está en la lista?
miElem :: (Eq a) => a -> [a] -> Bool
miElem _ [] = False
miElem e (x : xs)
  | e == x = True
  | otherwise = miElem e xs

-- 5. todosPares: Verifica si todos son pares (usando recursividad)
todosPares :: [Int] -> Bool
todosPares [] = True
todosPares (x : xs) = even x && todosPares xs

-- * Esto es el tipico and y ya esta funciona como eso

-- 6. todosAprobados: Suponiendo que aprueban con 6 o más
todosAprobados :: [Int] -> Bool
todosAprobados [] = True
todosAprobados (x : xs) = x >= 6 && todosAprobados xs

-- 9. Generalización (todosCumplen):
-- Crear una función genérica 'todosCumplen' que reciba un criterio (a -> Bool) y una lista.

todosCumplen :: (a -> Bool) -> [a] -> Bool
todosCumplen criterio [] = True
todosCumplen criterio (x : xs) = criterio x && todosCumplen criterio xs

--  todosCumplen (>2) [1,2] asi se usa
-- ! osea todosCumplen recibe una funcion que recibe un elemento y devuelve un bool, es una funcion de ORDEN SUPERIOR y ademas RECURSIVIDAD

-- 10. Filtrado (Filter):
-- Definir la función 'adultosMayores' que filtre personas con edad > 65 años usando 'filter'.

adultosMayores :: [Persona] -> [Persona]
adultosMayores = filter ((> 65) . edad)

adultosMayores' :: [Persona] -> [Persona]
adultosMayores' personas = filter esMayor personas

esMayor :: Persona -> Bool
esMayor persona = edad persona > 65

-- ! el filter = filter ( (aca_va_la_comparacion) . (aca_va_el_selector) )

-- 11. Cuantificadores (All y Any):
-- a) Implementar 'todosPrimos' usando la función 'all'.
-- b) Implementar 'algunoEsVengador' usando la función 'any'.

esPrimo :: Int -> Bool
esPrimo n = tieneDivisoresDesde 2 n -- Empezamos a probar desde el 2

tieneDivisoresDesde :: Int -> Int -> Bool
tieneDivisoresDesde divisor n
  | n == 1 = True
  | divisor == n = True -- Si llegué hasta el mismo número sin fallar, es primo
  | mod n divisor == 0 = False -- ¡Ups! Encontré a alguien que lo divide
  | otherwise = tieneDivisoresDesde (divisor + 1) n -- Pruebo con el siguiente

todosPrimos :: [Int] -> Bool
todosPrimos = all esPrimo

algunoPrimo :: [Int] -> Bool
algunoPrimo = any esPrimo

-- 12. ZipWith:
-- Utilizar 'zipWith' para realizar operaciones entre dos listas simultáneamente.

sumarElementosDeListas :: [Int] -> [Int] -> [Int]
sumarElementosDeListas [] _ = []
sumarElementosDeListas _ [] = []
sumarElementosDeListas (x : xs) (y : ys) = x + y : sumarElementosDeListas xs ys

-- 13. Lambdas y Funciones Anónimas:
-- a) Incrementar en 1 los elementos de una lista usando una lambda con map.
-- b) Crear 'otrasPalabras' que sume 'n' a la longitud de cada palabra en una lista.

incrementarLista' :: [Int] -> [Int]
incrementarLista' lista = map (\x -> x + 1) lista -- ! Lambda

incrementarEnUno :: [Int] -> [Int]
incrementarEnUno = map (+ 1) -- ! point free

otrasPalabras' :: [String] -> [String]
otrasPalabras' lista = map (\x -> x ++ "n") lista -- ! Lambda

-- 14. Alimentos y Calorías:
-- Dada una lista de 'InfoNutri', obtener los nombres de los alimentos que tienen hasta 100 calorías.

type Alimento = String

data InformacionNutricional = Info
  { alimento :: Alimento,
    calorias :: Int,
    grasas :: Float,
    carbohidratos :: Float,
    proteinas :: Float
  }
  deriving (Show, Eq)

infoManzana = Info "Manzana" 95 0.3 25.1 0.5

infoBanana = Info "Banana" 134 0.5 34.3 1.6

infoPera = Info "Pera" 101 0.2 27.1 0.6

infoEspinaca = Info "Espinaca" 7 0.1 1.1 0.9

infoYogurt = Info "Yogurt" 149 8.0 11.4 8.5

infoGarbanzos = Info "Garbanzos" 269 4.2 45.0 14.5

infosNutricionales =
  [ infoManzana,
    infoBanana,
    infoPera,
    infoEspinaca,
    infoYogurt,
    infoGarbanzos
  ]

alimentoParaDieta :: [InformacionNutricional] -> [Alimento]
alimentoParaDieta = map alimento . filter menosDe100

menosDe100 :: InformacionNutricional -> Bool
menosDe100 x = calorias x <= 99

-- 15. Foldeo (Foldr / Foldl):
-- a) Reescribir 'length', 'sum' y 'productoria' utilizando 'foldr'.
-- b) Implementar la búsqueda del máximo elemento de una lista usando foldeo.
-- c) Analizar la diferencia de asociatividad entre foldr1 y foldl1 con la operación resta (-).

-- 16. Análisis Complejo (Alimentos):
-- a) De los alimentos que NO son poco calóricos, verificar si alguno tiene más proteínas que grasas.
-- b) Identificar el alimento con mayor valor calórico o nombre más largo usando la función 'elDeMayor'.

-- 17. Composición con Foldeo:
-- Crear funciones para componer una lista de funciones ('componer') usando 'foldr1' y 'foldr'.

-- 18. Caso Práctico: Superhéroes:
-- A partir de un tipo de dato 'Carta':
-- a) Obtener nombres de cartas que comienzan con "bat".
-- b) Averiguar si hay cartas con etiquetas (tags) demasiado largos.
-- c) Corregir etiquetas erróneas (cambiar "#alguien" por "#alien").
import Distribution.PackageDescription (PackageDescription (subLibraries))

-- 1. Múltiplos:
-- Definir funciones para determinar si un número es múltiplo de dos, tres y diez.
-- Luego, generalizar en una función 'esMultiploDe' que reciba el divisor y el número.

esMultiploDeDos :: Int -> Bool
esMultiploDeDos x = mod x 2 == 0

esMultiploDeTres :: Int -> Bool
esMultiploDeTres x = mod x 3 == 0

-- ? a esto se le llama funcion de orden superior

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

alice :: Persona
alice = Persona "alice" 5

pepe :: Persona
pepe = Persona "pepe" 51

promedioDeEdad :: [Persona] -> Float
promedioDeEdad persons = sum (map edad persons) / fromIntegral (length persons)

-- 4. Doble de una lista:
-- Crear la función 'duplicarTodos' que calcule el doble de cada número en una lista.

duplicarLista :: (Num a) => [a] -> a
duplicarLista list = sum list

-- 5. Iniciales:
-- Obtener las iniciales de una lista de palabras (función 'iniciales').

-- 6. Implementación de Map (Recursividad):
-- Implementar manualmente la función 'map' usando patrones de lista (vacia y cabeza:cola).

-- 7. Refactorización con Map:
-- Reescribir las funciones 'edades', 'duplicarTodos' e 'iniciales' utilizando la función 'map'
-- y luego pasar a una definición con estilo tácito (point-free).

-- 8. Recursividad en Funciones Estándar:
-- Implementar manualmente las funciones: head, tail, null, elem, TodosPares y TodosAprobados.

-- 9. Generalización (todosCumplen):
-- Crear una función genérica 'todosCumplen' que reciba un criterio (a -> Bool) y una lista.

-- 10. Filtrado (Filter):
-- Definir la función 'adultosMayores' que filtre personas con edad > 65 años usando 'filter'.

-- 11. Cuantificadores (All y Any):
-- a) Implementar 'todosPrimos' usando la función 'all'.
-- b) Implementar 'algunoEsVengador' usando la función 'any'.

-- 12. ZipWith:
-- Utilizar 'zipWith' para realizar operaciones entre dos listas simultáneamente.

-- 13. Lambdas y Funciones Anónimas:
-- a) Incrementar en 1 los elementos de una lista usando una lambda con map.
-- b) Crear 'otrasPalabras' que sume 'n' a la longitud de cada palabra en una lista.

-- 14. Alimentos y Calorías:
-- Dada una lista de 'InfoNutri', obtener los nombres de los alimentos que tienen hasta 100 calorías.

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
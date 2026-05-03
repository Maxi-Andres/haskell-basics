-- EJERCICIO 1
-- Analizar la función int hacealgo(int a[], int b). Determinar qué hace el programa, cuál es el rol de cada variable (a, b, c, d) y cuánto tiempo toma deducir su funcionalidad

-- EJERCICIO 2
-- Refactorizar la función hacealgo para mejorar su expresividad. Cambiar nombres de variables y de la función para que el código sea autodocumentado, aplicar abstracción creando la función esPar y finalmente representar la misma lógica de forma declarativa usando filter y length

-- ! estos dos de arriba los hace el profe en el pdf, pasa que gemini no se dio cuenta

-- EJERCICIO 3
-- En un archivo llamado maderera.hs, definir las funciones: cuadruple (multiplica por 4), largoDeListon (constante 300), maderaCuadro (igual a cuadruple) y meAlcanza (compara un largo con el listón). Luego, definir puedoHacerCuadroDe mediante la composición de meAlcanza y maderaCuadro

-- cuadriple :: (Num a) => a -> a
-- cuadriple :: Int -> Int
cuadriple x = x * 4

largoDeListon :: Int -- !esto antes lo puse todo en una misma linea y funciono
largoDeListon = 300

maderaCuadro :: Int -> Int
maderaCuadro = cuadriple

meAlcanza :: Int -> Bool
meAlcanza x = x <= largoDeListon

puedoHacerCuadroDe :: Int -> Bool
puedoHacerCuadroDe = meAlcanza . maderaCuadro -- ! primero se hace la funcion de la derecha y despues la de la izquierda, es composicion de funciones

-- EJERCICIO 4
-- Modelar en el archivo maderera.hs la lógica necesaria para calcular qué porcentaje de listón sobraría para armar un cuadro de tamaño 50 de lado. Utilizar las funciones cuantoMeSobra y quePorcentaje mediante composición

cuantoMeSobra :: Int -> Int
cuantoMeSobra x = largoDeListon - x

quePorcentaje :: Int -> Float
quePorcentaje cantidad = (fromIntegral cantidad) / (fromIntegral largoDeListon) * 100

resultadoEjercicio4 :: Float -- haskell infiere tipos igual, asi que no hace falta esto
resultadoEjercicio4 = (quePorcentaje . cuantoMeSobra . maderaCuadro) 50 -- 50 es el valor de entrada de la primera funcion que se llama

-- EJERCICIO 5
-- Definir en Haskell las funciones esMayorDeEdad (edad >= 18) y esMenorDeEdad (edad < 18) con sus respectivos tipados. Definir una segunda versión de esMenorDeEdad que utilice la función esMayorDeEdad como base

-- * En los lenguajes imperativos (como C), usás return para "salir" de la función devolviendo un valor. En Haskell, que es un lenguaje funcional, una función es como una igualdad matemática.

-- * son como funciones matemáticas: tienen un Dominio (entrada) y una Imagen (salida). No hay un "proceso de recordar información" o pasos intermedios, solo valores que entran y valores que salen.

-- esMayorDeEdad :: Int -> Bool
-- esMayorDeEdad :: (Ord a, Num a) => a -> Bool
esMayorDeEdad edad = edad >= 18

esMenorDeEdad :: Int -> Bool
esMenorDeEdad edad = not (esMayorDeEdad edad)

-- EJERCICIO 6
-- Definir una función nombreFormateado que reciba un nombre y un apellido y devuelva el string con el formato "Apellido, Nombre"

nombreFormateado :: String -> String -> String
nombreFormateado nombre apellido = apellido ++ ", " ++ nombre -- ? tenes que poner las comillas cuando usas la funcion "", por que sino haskell piensa que es otra funcion o que es una "const" (que tambien es una funcion)

-- EJERCICIO 7
-- Implementar funciones de física para calcular la velocidad final (V = t * g) y la distancia recorrida (D = 1/2 * t^2 * g) de una pelota en caída libre a los 5 segundos, considerando g = 9.8. Determinar si la pelota rebota contra el piso si se lanza desde una altura de 80 metros

aceleracionDeLaGravedad :: Float
aceleracionDeLaGravedad = 9.807

velocidadFinalCaidaLibre :: Float -> Float
velocidadFinalCaidaLibre tiempo = tiempo * aceleracionDeLaGravedad

distanciaRecorridaCaidaLibre :: Float -> Float
distanciaRecorridaCaidaLibre tiempo = 1 / 2 * tiempo * tiempo * aceleracionDeLaGravedad

pasaLaAltura80 distancia = distancia >= 80

resultadoEjercicio5 = (pasaLaAltura80 . distanciaRecorridaCaidaLibre) 5

-- EJERCICIO 8
-- Definir la función color utilizando Pattern Matching para asociar: "banana" a "amarillo", "manzana" a "rojo" y "limon" a "amarillo". Verificar qué sucede al ingresar un valor no definido como "naranja"

color "banana" = "amarillo"
color "manzana" = "rojo"
color "limon" = "amarillo"
color cualquiera = "gris"

-- EXTRA

-- Verifica si una cantidad de listones se puede repartir equitativamente en parejas
esRepartible :: Int -> Bool
-- esRepartible cantidad = even cantidad
esRepartible = even

-- Calcula cuántos grupos completos de 3 personas se pueden armar
gruposDeTres :: Int -> Int
gruposDeTres personas = div personas 3

-- Verifica si una letra es una vocal (muy común en ejercicios de la UNSAM)
esVocal :: Char -> Bool
-- esVocal letra = elem letra "aeiouAEIOU"
esVocal letra = elem letra "aeiouAEIOU"

-- Invierte el formato de un código o nombre
invertirCadena :: String -> String
-- invertirCadena texto = reverse texto
invertirCadena = reverse

-- Calcula la hipotenusa de un triángulo (teorema de Pitágoras)
hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt (a ^ 2 + b ^ 2)

-- Redondea el costo final de un cuadro para no usar centavos
costoRedondeado :: Float -> Int
costoRedondeado costo = round costo

-- Obtiene solo el prefijo de un código de inventario (ej: los primeros 3 caracteres)
obtenerPrefijo :: String -> String
obtenerPrefijo codigo = take 3 codigo

-- Quita los primeros N elementos de una lista de medidas
quitarSobras :: Int -> [Int] -> [Int]
quitarSobras n medidas = drop n medidas

-- Útil para validar si un nombre de usuario es muy corto
esNombreCorto :: String -> Bool
esNombreCorto nombre = length nombre < 3

-- Función simple para sumar tres números usando (+)
sumarTres :: Int -> Int -> Int -> Int
sumarTres x y z = (+) ((+) x y) z

-- Útil en física para representar fuerzas en sentido contrario
fuerzaOpuesta :: Float -> Float
fuerzaOpuesta fuerza = negate fuerza

-- Verifica si un servidor NO está activo
estaCaido :: Bool -> Bool
estaCaido estaActivo = not estaActivo
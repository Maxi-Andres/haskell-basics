-- EJERCICIO 1: HORARIO DE SUPERMERCADO
-- Modelar la función horarioCierre que recibe el nombre de un día (Type Dia = String) y un booleano que indica si es feriado o no. Se deben cumplir las siguientes condiciones:
-- - Los domingos feriados cierra a las 13.
-- - Los sábados no feriados cierra a las 21.
-- - El resto de los feriados cierra a las 20.
-- - El resto de los días cierra a las 12 más la cantidad de letras que tiene el nombre del día.

type Dia = String

type Hora = Int

cierreSupermercado :: Dia -> Bool -> Hora
cierreSupermercado "Domingo" True = 13
cierreSupermercado "Sabado" False = 21
cierreSupermercado _ True = 20
cierreSupermercado dia False = 12 + length dia

-- EJERCICIO 2: CONJUNCIÓN CON VARIABLE ANÓNIMA
-- Definir la función Conjuncion (similar al operador &&) utilizando Pattern Matching. Primero realizar la definición por extensión para los cuatro casos posibles de True y False. Luego, refactorizar la función utilizando la variable anónima (_) para reducir la lógica a solo dos líneas, prestando especial atención al orden de las mismas.

-- conjuncion :: Bool -> Bool -> Bool
-- conjuncion True True = True
-- conjuncion True False = True
-- conjuncion False True = True
-- conjuncion False False = False

conjuncion :: Bool -> Bool -> Bool
conjuncion True True = True
conjuncion _ _ = False

-- EJERCICIO 3: DÍA DE LA SEMANA (FUNCIÓN PARCIAL)
-- Implementar la función diaDeSemana que recibe un Int (1 al 7) y devuelve el String del día correspondiente. Verificar qué sucede en la consola al ejecutar diaDeSemana 8 y analizar por qué se considera una función parcial.

diaDeSemana :: Int -> String
diaDeSemana 1 = "Lunes"
diaDeSemana 2 = "Martes"
diaDeSemana 3 = "Miercoles"
diaDeSemana 4 = "Jueves"
diaDeSemana 5 = "Viernes"
diaDeSemana 6 = "Sabado"
diaDeSemana 7 = "Domingo"

-- diaDeSemana _ = "No es un dia valido"

-- * se considera una función parcial porque no está definida para todos los valores posibles de su dominio.

-- EJERCICIO 4: FUNCIÓN PARTIDA CON GUARDAS
-- Definir la función f :: Int -> Int que utilice guardas para los siguientes casos:
-- - Si x < -1, devolver -1.
-- - Si x está entre -1 y 1 (inclusive), devolver 1.
-- - Si x > 1, devolver -1.
-- Luego, intentar reescribir la última condición utilizando la guarda 'otherwise'.

f :: Int -> Int
f x
  | x < -1 = -1
  | -1 <= x && x <= 1 = 1
  | otherwise = -1

-- ! Guardas vs Pattern Matching: el pattern matching define la función por extensión (caso por caso con valores concretos), mientras que las guardas la definen por condición (expresiones booleanas). Ambos pueden combinarse.

-- EJERCICIO 5: DIVISIÓN SEGURA
-- Definir la función Dividir :: Int -> Int -> Int. Utilizar guardas para asegurar que la operación solo se realice si el divisor es distinto de 0, excluyendo el 0 del dominio para evitar errores en tiempo de ejecución.

dividir :: Int -> Int -> Int
dividir dividendo divisor
  | divisor /= 0 = div dividendo divisor

-- Si divisor == 0, no hay resultado definido (función parcial intencional)

-- EJERCICIO 6: FACTORIAL Y FIBONACCI CON GUARDAS
-- Reimplementar las funciones Factorial y Fibonacci utilizando guardas en lugar de Pattern Matching puro. Asegurarse de manejar únicamente números mayores a 0 para evitar bucles infinitos.

calcularFactorial :: Int -> Int
calcularFactorial n
  | n == 0 = 1
  | n > 0 = calcularFactorial (n - 1) * n

secuenciaFibonacci :: Int -> Int
secuenciaFibonacci n
  | n == 0 = 0
  | n > 0 =
      secuenciaFibonacci (n - 1) + secuenciaFibonacci (n - 2)

-- EJERCICIO 7: TUPLAS Y TRUNCADO DE PALABRAS
-- Definir la función truncar que reciba una cantidad X y una palabra. La función debe devolver una tupla (dupla) que contenga:
-- 1. La palabra truncada a los primeros X caracteres.
-- 2. La cantidad de caracteres que fueron borrados de la palabra original.
-- Ejemplo de uso: truncar 3 "Hola" debe devolver ("Hol", 1).

type PalabraTruncada = (String, Int)

truncarPalabra :: Int -> String -> PalabraTruncada
truncarPalabra x palabra = (take x palabra, length palabra - x)

-- EJERCICIO 8: MODELADO DE ESTUDIANTES (DATA)
-- Crear un tipo de dato Estudiante usando la sintaxis 'data' con los campos: nombre (String), legajo (String) y nota (Int). Realizar las siguientes funciones:
-- - aprobo: devuelve True si la nota es mayor o igual a 7.
-- - legajoyNombre: devuelve el legajo y el nombre concatenados (usando Pattern Matching sobre el constructor).
-- - lefueMejorA: recibe dos estudiantes y devuelve True si el primero tiene mejor nota que el segundo (realizar una versión con Pattern Matching y otra usando las funciones de acceso).

-- * es como un struct de C o dataType en Kotlin,

data Estudiante = Estudiante -- el primer dato es el nombre de el tipo el segundo es el contructor como en C
  { nombre :: String,
    legajo :: String,
    nota :: Int
  }
  deriving (Show, Eq) -- ! sin esto tiran error las funciones, por que al usarlas tienen que devolver algo y sin esto no sabe como imprimirlas por pantalla, con estas dos se puede MOSTRAR Y COMPARAR

juanita :: Estudiante
juanita = Estudiante {nombre = "juanita", legajo = "L004100-3", nota = 8}

pepito :: Estudiante
pepito = Estudiante "pepito" "L004101-4" 6

aproboEstudiante :: Estudiante -> Bool
aproboEstudiante estudiante = nota estudiante >= 7 -- ! asi se accede a las "variables" internas

legajoyNombre :: Estudiante -> String
-- legajoyNombre estudiante = nombre estudiante ++ " " ++ legajo estudiante
legajoyNombre (Estudiante nom leg _) = leg ++ ", " ++ nom

-- version con pattern matching
leFueMejorA :: Estudiante -> Estudiante -> Bool
leFueMejorA (Estudiante _ _ unaNota) (Estudiante _ _ otraNota) = unaNota > otraNota

-- Versión más abstracta (sin exponer la implementación interna):
leFueMejorA' :: Estudiante -> Estudiante -> Bool
leFueMejorA' estu1 estu2 = nota estu1 > nota estu2

-- EJERCICIO 9: INMUTABILIDAD Y CAMBIO DE ESTADO
-- Definir la función cambiarNota que reciba una nueva nota y un Estudiante, y devuelva un "nuevo" Estudiante con la nota actualizada. Luego definir subirNota que utilice cambiarNota para incrementar en 1 la nota actual de un alumno.

cambiarNota :: Int -> Estudiante -> Estudiante
cambiarNota newNota (Estudiante nombre legajo _) = Estudiante nombre legajo newNota

subirNota :: Estudiante -> Estudiante
subirNota estudiante = cambiarNota (nota estudiante + 1) estudiante -- ! esto solo le sube 1 acordate que las "variables" son inmutables

-- EJERCICIO 10: DERIVING (SHOW Y EQ)
-- Modificar el tipo Estudiante agregando 'deriving (Show, Eq)'. Explicar qué permite hacer cada una de estas extensiones (mostrar en consola y comparar estudiantes con ==).

-- * hecho arriba

-- EXTRA

queAlumno :: Estudiante -> String
queAlumno (Estudiante _ _ 7) = "Sabe Algo" -- Pattern Matching exacto
queAlumno estudiante
  | nota estudiante > 7 = "Genio" -- Guarda para condiciones
  | otherwise = "Si que Sabe" -- Caso por defecto (Totalidad)

queAlumno' :: Estudiante -> String
queAlumno' (Estudiante _ _ n)
  | n > 7 = "Genio"
  | n == 7 = "Sabe Algo"
  | otherwise = "Si que Sabe"

data Persona = Persona
  { name :: String,
    edad :: Int,
    barrio :: String
  }

juan = ("Juan", 9, "Haedo")

gonza = ("Pedro", 9, "Boedo")

maria = ("Maria", 10, "Boedo")

type Grupo = [Persona]

personas = [juan, gonza, maria]

-- head [1, 2, 3] -- 1          (primer elemento)
-- tail [1, 2, 3] -- [2,3]      (todo menos el primero)
-- last [1, 2, 3] -- 3          (último elemento)
-- length [1, 2, 3] -- 3
-- reverse [1, 2, 3] -- [3,2,1]
-- take 2 [1, 2, 3] -- [1,2]
-- sum [1, 2, 3] -- 6
-- maximum [1, 2, 3] -- 3
-- minimum [1, 2, 3] -- 1
-- [1, 2] ++ [3, 4] -- [1,2,3,4]  (concatenación)
-- 1 : [2, 3] -- [1,2,3]    (agregar al frente)
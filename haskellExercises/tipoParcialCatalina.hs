data Contenido = UnContenido
  { titulo :: String,
    generos :: [String],
    duracion :: Int, -- En minutos
    esPremium :: Bool
  }
  deriving (Show, Eq)

type Usuario = String

data Historial = UnHistorial
  { usuario :: Usuario,
    esVip :: Bool,
    contenidosVistos :: [Contenido]
  }
  deriving (Show, Eq)

-- ! Haskell, por atrás y en secreto, agarra esa palabra contenidosVistos y fabrica automáticamente una función con esta firma: contenidosVistos :: Historial -> [Contenido] Es decir: crea una función que se llama contenidosVistos, que espera que le pases un Historial por argumento, lo rompe, le extrae la lista de contenidos que tiene adentro y te la devuelve.

-- Datos de prueba para guiarte:
catalogo :: [Contenido]
catalogo =
  [ UnContenido "Inception" ["Sci-Fi", "Thriller"] 148 True,
    UnContenido "The Office" ["Comedy"] 22 False,
    UnContenido "Interstellar" ["Sci-Fi", "Drama"] 169 True,
    UnContenido "Shrek" ["Comedy", "Family"] 90 False
  ]

pene = UnContenido "Shrek" ["Comedy", "Family"] 90 False

historialVip = UnHistorial "pepe" True catalogo

-- Definir la función esMaratonDeComedia que determine si un contenido dura menos de 30 minutos y pertenece al género "Comedy".

esMaratonDeComedia :: Contenido -> Bool
esMaratonDeComedia contenido = duracion contenido < 30 && elem "Comedy" (generos contenido)

-- Definir la función puedeVer que reciba un Historial de usuario y un Contenido. Un usuario puede ver el contenido si el contenido no es premium, o si el usuario es VIP.

puedeVer :: Historial -> Contenido -> Bool
puedeVer historial contenido = esVip historial || not (esPremium contenido)

-- Implementar la función recomendacionesPorGenero que, dado un género musical/cinematográfico y una lista de contenidos (el catálogo), devuelva únicamente los títulos de los contenidos que pertenecen a ese género.

recomendacionesPorGenero :: String -> [Contenido] -> [String]
recomendacionesPorGenero genero listaContenidos = map titulo (filter (elem genero . generos) listaContenidos)

-- Definir la función esFanaticoDe que reciba un género, un Historial de usuario y determine si todos los contenidos que vio en su historial pertenecen a ese género.

esFanaticoDe :: String -> Historial -> Bool
esFanaticoDe genero historial = all (elem genero . generos) (contenidosVistos historial)

-- Implementar tiempoTotalVisto que calcule la suma de las duraciones de todos los contenidos que un usuario tiene en su historial.

tiempoTotalVisto :: Historial -> Int
tiempoTotalVisto historial = sumarTiempoDeVista (contenidosVistos historial)

sumarTiempoDeVista :: [Contenido] -> Int
sumarTiempoDeVista [] = 0
sumarTiempoDeVista (x : xs) = duracion x + sumarTiempoDeVista xs

-- Para cerrar el estilo de parcial, respondé a las siguientes preguntas basándote en el código que vas a escribir y los conceptos de Evaluación Eager (Estrictamente evaluada) y Evaluación Lazy (Perezosa):

-- Si tuviéramos un catálogo infinito de películas de Ciencia Ficción generado recursivamente (por ejemplo, con repeat o iterate), ¿qué pasaría al ejecutar la función recomendacionesPorGenero "Sci-Fi" sobre esa lista infinita en Haskell? ¿Y qué pasaría en un lenguaje con evaluación Eager (como JavaScript o Python)?

-- Imaginá que modificamos la función puedeVer del punto 2. Si el usuario es VIP, la función devuelve True inmediatamente sin mirar si la película es Premium o no. Si le pasamos un contenido cuya estructura de datos rompe con un error (ej: undefined), ¿cómo se comportaría Haskell gracias a su evaluación Lazy si el usuario es VIP?

-- Respuestas Teóricas (Estrategias de Evaluación)
-- 1. El catálogo infinito
-- En Haskell (Lazy Evaluation): La función recomendacionesPorGenero se colgaría (entraría en un bucle infinito). ¿Por qué? Porque filter tiene que evaluar toda la lista para saber qué elementos se quedan y cuáles se van. Al ser la lista infinita, el filter se queda eternamente buscando elementos que cumplan la condición, por lo que nunca termina de entregarle la lista resultante a map. (Ojo: Haskell es Lazy, pero si le pedís filtrar una lista infinita, no puede saber el final).

-- En lenguajes Eager (Python/JavaScript): Se colgaría inmediatamente al intentar crear la lista infinita en memoria. Ni siquiera llegaría a ejecutar la función de recomendación, porque el programa se quedaría sin memoria (Stack Overflow o Out of Memory) intentando evaluar el parámetro infinito.

-- 2. El usuario VIP y el undefined
-- Comportamiento en Haskell: La función puedeVer devolverá True y no tirará ningún error.

-- Explicación: Debido a la evaluación Lazy, Haskell solo evalúa lo que necesita de manera estricta. En la función puedeVer usamos un operador lógico || (OR). Como la primera parte de la condición (esVip unHistorial) ya da True, por definición del cortocircuito lógico del OR, el resultado global ya es True. Haskell "sabe" que no necesita evaluar el segundo operando, por lo que el undefined del contenido jamás se ejecuta ni rompe el programa.
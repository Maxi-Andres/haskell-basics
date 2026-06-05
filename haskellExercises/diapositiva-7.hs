-- ## Paradigmas de Programación Funcional: Caso Librería
-- A partir del siguiente modelo de datos:
-- data Obra = UnaObra { titulo:: Titulo, autores:: [Autor] }
-- type Titulo = String
-- type Autor = String

data Obra = UnaObra
  { titulo :: Titulo,
    autores :: [Autor]
  }

type Titulo = String

type Autor = String

todasLasObras :: [Obra]
todasLasObras =
  [ UnaObra "sandman" ["gaiman"],
    UnaObra "socorro" ["Elsa Bornemann"],
    UnaObra "buenos presagios" ["gaiman", "pratchett"]
  ]

-- 1. Verificar Autoría:
-- Definir la función 'escribio' que determine si un autor específico escribió una obra con un título determinado.
-- escribio :: Autor -> Titulo -> [Obra] -> Bool

escribioGuardas :: Autor -> Titulo -> [Obra] -> Bool
escribioGuardas _ _ [] = False
escribioGuardas auth tit (x : xs)
  | tit == titulo x = encontrarAutor x auth
  | otherwise = escribioGuardas auth tit xs

encontrarAutor :: Obra -> Autor -> Bool
encontrarAutor obra elEscritor = elem elEscritor (autores obra)

escribio' :: Autor -> Titulo -> [Obra] -> Bool
escribio' unAutor unTitulo todasobras = any (tieneAutorYTitulo' unAutor unTitulo) todasobras

-- ! any (condición) data
-- ! ej any (1==) [0,1,2,3,4,5]

tieneAutorYTitulo' :: Autor -> Titulo -> Obra -> Bool
tieneAutorYTitulo' unAutor unTitulo unaObra = titulo unaObra == unTitulo && elem unAutor (autores unaObra)

-- 2. Autores de una Obra:
-- ¿Quién o quienes escribieron una obra? Implementar 'autoresDe' para obtener la lista de autores de un título.
-- autoresDe :: Titulo -> [Obra] -> [Autor]

autoresDe :: Titulo -> [Obra] -> [Autor]
autoresDe unTit lasObras = autores (head (filter (encontrarObra unTit) lasObras))

encontrarObra :: Titulo -> Obra -> Bool
encontrarObra unTit unaObra = unTit == titulo unaObra

autoresDe' :: Titulo -> [Obra] -> [Autor]
autoresDe' unTitulo todasobras = autores (head (filter ((== unTitulo) . titulo) todasobras))

-- 3. Obras de un Autor:
-- ¿Qué obra escribió cierta persona? Implementar 'obrasDe' que devuelva los títulos de las obras de un autor.
-- obrasDe :: Autor -> [Obra] -> [Titulo]

obrasDe :: Autor -> [Obra] -> [Titulo]
obrasDe auth obras = map titulo (filter (filtrarPorAuthor auth . autores) obras)

filtrarPorAuthor :: Autor -> [Autor] -> Bool
filtrarPorAuthor auth autores = elem auth (autores)

obrasDe' :: Autor -> [Obra] -> [Titulo]
obrasDe' auth obras = map titulo (filter (elem auth . autores) obras)

-- 4. Producción del Autor (escribioAlgo):
-- Verificar si una persona escribió alguna obra, sin importar cuál.
-- escribioAlgo :: Autor -> [Obra] -> Bool

escribioAlgo :: Autor -> [Obra] -> Bool
escribioAlgo autor obras = any (filtrarPorAuthor autor . autores) obras

escribioAlgo' :: Autor -> [Obra] -> Bool
escribioAlgo' autor listaObras = any (elem autor . autores) listaObras

-- 5. Existencia de Obra:
-- Determinar si es cierto que una obra con un título determinado existe en la base de datos.
-- existe :: Titulo -> [Obra] -> Bool

existeObra :: Titulo -> [Obra] -> Bool
existeObra unTit lasObras = any ((== unTit) . titulo) lasObras

existeObra' :: Titulo -> [Obra] -> Bool
existeObra' unTitulo listaObras = any (\obra -> titulo obra == unTitulo) listaObras

-- 6. Estrategias de Evaluación (Teórico/Práctico):
-- Analizar cómo se comportan las funciones anteriores bajo Evaluación Eager vs Lazy.
-- Eager: Evalúa parámetros primero, luego la función.
-- Lazy: Retrasa la evaluación de parámetros hasta que no tiene otra opción.
-- Ejemplo: ¿Qué pasa con una lista infinita en Haskell?
-- head (iterate (*2) 1)
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

escribio :: Autor -> Titulo -> [Obra] -> Bool
escribio autor titulo obras = any (tieneAutorYTitulo autor titulo) obras

tieneAutorYTitulo :: Autor -> Titulo -> Obra -> Bool
tieneAutorYTitulo autor untitulo obra = titulo obra == untitulo && elem autor (autores obra)

-- 2. Autores de una Obra:
-- ¿Quién o quienes escribieron una obra? Implementar 'autoresDe' para obtener la lista de autores de un título.
-- autoresDe :: Titulo -> [Obra] -> [Autor]

autoresDe :: Titulo -> [Obra] -> [Autor]
autoresDe unTitulo listaObras = autores (head (filter ((== unTitulo) . titulo) listaObras))

-- 3. Obras de un Autor:
-- ¿Qué obra escribió cierta persona? Implementar 'obrasDe' que devuelva los títulos de las obras de un autor.
-- obrasDe :: Autor -> [Obra] -> [Titulo]

obrasDe :: Autor -> [Obra] -> [Titulo]
obrasDe autor listaObras = map titulo (filter (filtrarAutor autor . autores) listaObras)

filtrarAutor :: Autor -> [Autor] -> Bool
filtrarAutor autor listaAutores = elem autor listaAutores

-- 4. Producción del Autor (escribioAlgo):
-- Verificar si una persona escribió alguna obra, sin importar cuál.
-- escribioAlgo :: Autor -> [Obra] -> Bool

escribioAlgo :: Autor -> [Obra] -> Bool
escribioAlgo autor listaObras = any (elem autor . autores) listaObras

-- 5. Existencia de Obra:
-- Determinar si es cierto que una obra con un título determinado existe en la base de datos.
-- existe :: Titulo -> [Obra] -> Bool
existe :: Titulo -> [Obra] -> Bool
existe unTitulo listaObras = any (\obra -> titulo obra == unTitulo) listaObras

-- 6. Estrategias de Evaluación (Teórico/Práctico):
-- Analizar cómo se comportan las funciones anteriores bajo Evaluación Eager vs Lazy.
-- Eager: Evalúa parámetros primero, luego la función.
-- Lazy: Retrasa la evaluación de parámetros hasta que no tiene otra opción.
-- Ejemplo: ¿Qué pasa con una lista infinita en Haskell?
-- head (iterate (*2) 1)
data Obra = UnaObra
  { titulo :: Titulo,
    autores :: [Autor],
    genero :: Genero,
    paginas :: Int
  }
  deriving (Show, Eq)

type Titulo = String

type Autor = String

type Genero = String -- Ej: "Terror", "Fantasía", "Policial"

todasLasObras :: [Obra]
todasLasObras =
  [ UnaObra {titulo = "sandman", autores = ["gaiman"], genero = "Accion", paginas = 120},
    UnaObra {titulo = "socorro", autores = ["Elsa Bornemann"], genero = "Terror", paginas = 150},
    UnaObra {titulo = "buenos presagios", autores = ["gaiman", "pratchett"], genero = "Comedia", paginas = 200},
    UnaObra {titulo = "hailmary", autores = ["pratchett"], genero = "Comedia", paginas = 220},
    UnaObra {titulo = "la ultima obra", autores = ["gaiman"], genero = "Hot", paginas = 320}
  ]

-- Queremos saber si dos autores suelen trabajar juntos. Definir la función colaboraron :: Autor -> Autor -> [Obra] -> Bool que devuelve True si existe al menos una obra en la lista donde ambos autores hayan participado.

dosAutoresColaboraron :: Autor -> Autor -> [Obra] -> Bool
dosAutoresColaboraron autor1 autor2 listaDeObras = any (obraTieneAutores autor1 autor2 . autores) listaDeObras

obraTieneAutores :: Autor -> Autor -> [Autor] -> Bool
obraTieneAutores autor1 autor2 listaDeAutores = elem autor1 listaDeAutores && elem autor2 listaDeAutores

-- Definir la función totalPaginasDe :: Autor -> [Obra] -> Int. Dada la lista de obras, debe calcular cuántas páginas en total escribió un autor sumando las páginas de todas las obras en las que participó.
-- Restricción: Trata de resolverlo utilizando foldl o foldr en algún punto de tu solución para practicar abstracción de la reducción.

totalDePaginasEscritasPor :: Autor -> [Obra] -> Int
totalDePaginasEscritasPor autor listaDeObras = foldl1 (+) (map paginas (filter (elem autor . autores) listaDeObras))

totalDePaginasEscritasPor2 :: Autor -> [Obra] -> Int
totalDePaginasEscritasPor2 autor listaDeObras = foldl (\acumulador obra -> acumulador + paginas obra) 0 (filter (elem autor . autores) listaDeObras)

totalDePaginasEscritasPor' :: Autor -> [Obra] -> Int
totalDePaginasEscritasPor' autor listaDeObras = sum (map paginas (filter (elem autor . autores) listaDeObras))

-- Necesitamos estadísticas de la librería. Definir la función resumenDeGenero :: Genero -> [Obra] -> (Int, Int) que dada un género y la lista de obras, devuelva una tupla donde:
-- El primer elemento sea la cantidad total de obras de ese género.
-- El segundo elemento sea la página máxima encontrada en una obra de ese género (si no hay obras de ese género, puede devolver 0).

resumenDeGenero :: Genero -> [Obra] -> (Int, Int)
resumenDeGenero unGenero todasLasObras
  | null obrasDelGenero = (0, 0)
  | otherwise = (length obrasDelGenero, maximum (map paginas obrasDelGenero))
  where
    obrasDelGenero = filter (\obra -> genero obra == unGenero) todasLasObras

-- Un autor se considera "prolífico" si escribió más de 2 obras o si al menos una de sus obras tiene más de 500 páginas. Definir la función esProlifico :: Autor -> [Obra] -> Bool.
-- Tip: Reutilizá funciones que ya tengas o modularizá con funciones helper cortas.

autorEsProlifico :: Autor -> [Obra] -> Bool
autorEsProlifico autor listaDeObras = length obrasDelAutor > 2 || any (\obra -> paginas obra > 500) obrasDelAutor
  where
    obrasDelAutor = filter (elem autor . autores) listaDeObras

-- el where te permiete crear una "variable" local para no repetir el filter
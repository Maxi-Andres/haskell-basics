-- Se tiene un sistema para gestionar festivales de música. Cada festival tiene un nombre, una ciudad y una lista de bandas que tocan. Se sabe lo siguiente:
data Festival = UnFestival
  { nombreFestival :: String,
    ciudad :: String,
    bandas :: [String]
  }
  deriving (Show, Eq)

-- Y se cuenta con la siguiente lista de festivales:

festivales :: [Festival]
festivales =
  [ UnFestival "Lollapalooza" "Buenos Aires" ["The Killers", "Blur", "Blink-182"],
    UnFestival "Primavera Sound" "Barcelona" ["Blur", "Portishead", "LCD Soundsystem"],
    UnFestival "Cosquin Rock" "Cordoba" ["Los Piojos", "Guasones", "Blur"],
    UnFestival "Rock in Rio" "Rio" ["Iron Maiden", "Guns N Roses", "The Killers"]
  ]

-- Implementá las siguientes funciones:

-- a) festivalesEn :: String -> [Festival] -> [String]
-- Dado un nombre de ciudad, devuelve los nombres de los festivales que se realizan en esa ciudad.

festivalesEn :: String -> [Festival] -> [String]
festivalesEn nombreCiudad listaFestivales = map nombreFestival (filter ((== nombreCiudad) . ciudad) listaFestivales)

-- b) bandaPresente :: String -> [Festival] -> Bool
-- Dado el nombre de una banda, indica si toca en al menos uno de los festivales.

bandaPresente :: String -> [Festival] -> Bool
bandaPresente nombreBanda listaFestivales = any (elem nombreBanda . bandas) listaFestivales

-- c) festivalesConBanda :: String -> [Festival] -> [String]
-- Dado el nombre de una banda, devuelve los nombres de los festivales donde esa banda toca.

festivalesConBanda :: String -> [Festival] -> [String]
festivalesConBanda nombreBanda listaFestivales = map nombreFestival (filter (elem nombreBanda . bandas) listaFestivales)

-- d) todasLasBandas :: [Festival] -> [String]
-- Devuelve la lista de todas las bandas que tocan en al menos un festival (pueden repetirse).

todasLasBandas2 :: [Festival] -> [String]
todasLasBandas2 [] = []
todasLasBandas2 (x : xs) = bandas x ++ todasLasBandas2 xs

todasLasBandas :: [Festival] -> [[String]]
todasLasBandas [] = []
todasLasBandas (x : xs) = bandas x : todasLasBandas xs

-- e) granFestival :: Festival -> Bool
-- Un festival es "gran festival" si tiene más de dos bandas. Luego definí grandesFestivales :: [Festival] -> [String] que devuelve los nombres de todos los grandes festivales.

granFestival :: Festival -> Bool
granFestival festival = length (bandas festival) > 2

grandesFestivales :: [Festival] -> [String]
grandesFestivales listaFestivales = map nombreFestival (filter (granFestival) listaFestivales)
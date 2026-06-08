data Atleta = UnAtleta
  { nombre :: String,
    edad :: Int,
    pulsaciones :: Int,
    ejerciciosHechos :: [String]
  }
  deriving (Show, Eq)

type Gimnasio = [Atleta]

-- Lista de ejemplo para que puedas hacer pruebas en tu cabeza o en GHCi:
gymUNSAM :: Gimnasio
gymUNSAM =
  [ UnAtleta "Cata" 22 80 ["Sentadillas", "Cinta"],
    UnAtleta "Fer" 28 140 ["Cinta", "Spinning", "Plancha"],
    UnAtleta "Mati" 19 65 ["Sentadillas"]
  ]

pepe = UnAtleta "pepe" 99 60 ["Sentadillas"]

--   El gimnasio necesita clasificar el estado de un atleta según sus pulsaciones actuales. Definir la función estadoCardiaco :: Atleta -> String que devuelva:
-- "Reposo" si tiene menos de 70 pulsaciones.
-- "Aeróbico" si tiene entre 70 y 130 pulsaciones (inclusive).
-- "Exigido" si tiene más de 130 pulsaciones.

estadoCardiaco :: Atleta -> String
estadoCardiaco atleta
  | pulsaciones atleta < 70 = "Reposo"
  | pulsaciones atleta <= 130 = "Aerobico"
  | otherwise = "Exigido"

--   Queremos simular el efecto de que un grupo de atletas corra en la cinta. Cuando un atleta hace cinta, se le suma "Cinta" a su lista de ejercicios hechos y sus pulsaciones aumentan en 20.
-- Definir la función diaDeCardio :: Gimnasio -> Gimnasio que reciba a todos los atletas del gimnasio y devuelva la lista con las modificaciones aplicadas a cada uno.
-- Tip: Te conviene armar primero una función helper hacerCinta :: Atleta -> Atleta.

hacerCinta' :: Atleta -> Atleta
hacerCinta' (UnAtleta nombre edad pulsaciones ejerciciosHechos) = UnAtleta {nombre = nombre, edad = edad, pulsaciones = pulsaciones + 20, ejerciciosHechos = ejerciciosHechos}

hacerCinta :: Atleta -> Atleta
hacerCinta unAtleta =
  unAtleta
    { pulsaciones = pulsaciones unAtleta + 20,
      ejerciciosHechos = ejerciciosHechos unAtleta ++ ["Cinta"]
    }

diaDeCardio :: Gimnasio -> Gimnasio
diaDeCardio unGimnasio = map hacerCinta unGimnasio

-- Queremos premiar al atleta que más constancia tiene. Definir la función elMasEntrenado :: Gimnasio -> Atleta que devuelva al atleta que haya realizado la mayor cantidad de ejercicios en su historial.
-- Si hay un empate, puede devolver cualquiera de los que empataron.
-- Restricción: No vale usar maximum sobre un mapeo. Tenés que usar foldl1 o foldr1 para comparar a los atletas entre sí y "quedarte" con el que tenga la lista de ejercicios más larga.

elMasEntrenado :: Gimnasio -> Atleta
elMasEntrenado unGimnasio = foldl1 elegirMasEntrenado unGimnasio

elegirMasEntrenado :: Atleta -> Atleta -> Atleta
elegirMasEntrenado atleta1 atleta2
  | length (ejerciciosHechos atleta1) >= length (ejerciciosHechos atleta2) = atleta1
  | otherwise = atleta2

-- El seguro del gimnasio nos pide reportar si hay peligro en las instalaciones. Definir la función hayAlertaDeRiesgo :: Gimnasio -> Bool que devuelva True si al menos uno de los atletas del gimnasio cumple dos condiciones en simultáneo: tiene más de 25 años y está "Exigido" (podés reutilizar tu función del punto 1).

hayAlertaDeRiesgo :: Gimnasio -> Bool
hayAlertaDeRiesgo unGimnasio = any estaEnRiesgo unGimnasio

estaEnRiesgo :: Atleta -> Bool
estaEnRiesgo unAtleta = edad unAtleta > 25 && estadoCardiaco unAtleta == "Exigido"
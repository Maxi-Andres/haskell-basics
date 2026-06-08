import Control.Exception (BlockedIndefinitelyOnMVar)
import Distribution.Simple.Glob (fileGlobMatches)
import System.Directory (Permissions)

data Personaje = UnPersonaje
  { nombre :: String,
    clase :: String, -- Ej: "Guerrero", "Mago", "Pícaro"
    nivel :: Int,
    oro :: Int,
    habilidades :: [String]
  }
  deriving (Show, Eq)

type Party = [Personaje]

-- Lista de ejemplo para pruebas:
partyUNSAM :: Party
partyUNSAM =
  [ UnPersonaje "Aragorn" "Guerrero" 15 150 ["Ataque Poderoso", "Liderazgo"],
    UnPersonaje "Gandalf" "Mago" 20 500 ["Bola de Fuego", "Teletransportacion", "Luz"],
    UnPersonaje "Legolas" "Picaro" 12 80 ["Disparo Doble"],
    UnPersonaje "Gimli" "Guerrero" 14 300 ["Golpe de Hacha"]
  ]

-- 1. El Botín del Dragón (map)
-- La party derrotó a un jefe y encontró un gran botín. Definir la función repartirOro :: Int -> Party -> Party que reciba una cantidad de oro total y la party. Debe devolver la party modificada, sumándole a cada personaje una parte igual de ese oro (asumí que la división da un número entero exacto, podés usar div).

repartirOro :: Int -> Party -> Party
repartirOro oro party = map (sumarOro (dividirOro oro party)) party

dividirOro :: Int -> Party -> Int
dividirOro oro party = div oro (length party)

sumarOro :: Int -> Personaje -> Personaje
sumarOro oroAgregar unPersonaje = unPersonaje {nombre = nombre unPersonaje, clase = clase unPersonaje, nivel = nivel unPersonaje, oro = oro unPersonaje + oroAgregar, habilidades = habilidades unPersonaje}

-- 2. Taberna Exclusiva (filter)
-- En una ciudad hay una taberna donde solo dejan entrar a héroes experimentados. Definir la función puedenEntrar :: Party -> [String] que reciba la party y devuelva solo los nombres de los personajes que cumplen al menos una de estas dos condiciones:
-- Tener nivel mayor a 14.
-- Saber la habilidad "Bola de Fuego".

puedenEntrar :: Party -> [String]
puedenEntrar party = map nombre ((filter tabernaExclusiva) party)

tabernaExclusiva :: Personaje -> Bool
tabernaExclusiva personaje = (nivel personaje) > 14 || elem "Bola de Fuego" (habilidades personaje)

-- 3. El Gremio de Magos (all / any)
-- Queremos saber si la party está lista para una misión del Gremio de Magos. Definir la función misionMagicaApta :: Party -> Bool que devuelva True si todos los magos de la party (aquellos cuya clase sea "Mago") tienen un nivel superior a 15.
-- Ojo con la trampa: Si no hay magos en la party, la función debería devolver True (comportamiento estándar de all).

misionMagicaApta :: Party -> Bool
misionMagicaApta party = all (\mago -> nivel mago > 15) (filtrarMagos party)

filtrarMagos :: Party -> Party
filtrarMagos party = filter ((== "Mago") . clase) party

-- 4. Libro de Hechizos Total (concat / concatMap)
-- Definir la función repertorioDeHabilidades :: Party -> [String] que devuelva una única lista plana con todas las habilidades de todos los integrantes de la party juntas.
-- Tratá de resolverlo con el enfoque declarativo que venís usando (composición ., aplicación parcial, etc.) y prestando atención a no tropezarte con el aplanado de listas en el punto 4.

repertorioDeHabilidades :: Party -> [String]
repertorioDeHabilidades [] = []
repertorioDeHabilidades (x : xs) = habilidades x ++ repertorioDeHabilidades xs

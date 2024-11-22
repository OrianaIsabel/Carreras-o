module Library where
import PdePreludat

data Auto = UnAuto {
    color :: String,
    velocidad :: Number,
    distancia :: Number
} deriving (Show, Eq)

blanco :: Auto
blanco = UnAuto "Blanco" 120 0

rojo :: Auto
rojo = UnAuto "Rojo" 120 0

negro :: Auto
negro = UnAuto "Negro" 120 0

azul :: Auto
azul = UnAuto "Azul" 120 0

rosa :: Auto
rosa = UnAuto "Rosa" 300 40

verde :: Auto
verde = UnAuto "Verde" 400 10

morado :: Auto
morado = UnAuto "Morado" 200 15

nascar :: Carrera
nascar = [azul, rojo, blanco, negro]

type Carrera = [Auto]

-- Punto 1

distanciaEntre :: Auto -> Auto -> Number
distanciaEntre auto1 auto2 = abs ((distancia auto1) - (distancia auto2))

estanCerca :: Auto -> Auto -> Bool
estanCerca auto1 auto2 = (auto1 /= auto2) && (< 10) (distanciaEntre auto1 auto2)

estaSolo :: Auto -> Carrera -> Bool
estaSolo auto carrera = all (not.(estanCerca auto)) carrera

vaPrimero :: Auto -> Carrera -> Bool
vaPrimero auto carrera = all ((<= (distancia auto)).distancia) carrera

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = (estaSolo auto carrera) && (vaPrimero auto carrera)

leEstaGanando :: Auto -> Auto -> Bool
leEstaGanando ganador perdedor = (distancia ganador) > (distancia perdedor)

quienLeGana :: Auto -> Carrera -> [Auto]
quienLeGana auto carrera = filter (flip leEstaGanando auto) carrera

puesto :: Auto -> Carrera -> Number
puesto auto carrera = (+ 1) (length (quienLeGana auto carrera))

-- Punto 2

correr :: Auto -> Number -> Auto
correr auto tiempo = auto {distancia = (distancia auto) + tiempo * (velocidad auto)}

type Modificador = Number -> Number

alterarVelocidad :: Auto -> Modificador -> Auto
alterarVelocidad auto modificador = auto {velocidad = modificador (velocidad auto)}

bajarVelocidad :: Auto -> Number -> Auto
bajarVelocidad auto cantidad = alterarVelocidad auto ((max 0).subtract cantidad)

-- Punto 3

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto auto carrera = afectarALosQueCumplen (estanCerca auto) (flip bajarVelocidad 50) carrera

miguelitos :: Number -> PowerUp
miguelitos cantidad auto carrera = afectarALosQueCumplen (leEstaGanando auto) (flip bajarVelocidad cantidad) carrera

jetPack :: Number -> PowerUp
jetPack tiempo auto carrera = afectarALosQueCumplen (== auto) ((flip correr tiempo).(flip alterarVelocidad (* 2))) carrera

-- Punto 4

type Evento = Carrera -> Carrera

correnTodos :: Number -> Evento
correnTodos tiempo carrera = map (flip correr tiempo) carrera

usarPowerUp :: PowerUp -> Auto -> Evento
usarPowerUp powerUp auto carrera = powerUp auto carrera

simularEvento :: Carrera -> Evento -> Carrera
simularEvento carrera evento = evento carrera

simularEventos :: Carrera -> [Evento] -> Carrera
simularEventos carrera eventos = foldl simularEvento carrera eventos

celdaPosicion :: Auto -> Carrera -> (Number, String)
celdaPosicion auto carrera = (puesto auto carrera, color auto)

tablaPosiciones :: Carrera -> [(Number, String)]
tablaPosiciones carrera = map (flip celdaPosicion carrera) carrera

simularCarrera :: Carrera -> [Evento] -> [(Number, String)]
simularCarrera carrera eventos = tablaPosiciones (simularEventos carrera eventos)

eventos :: [Evento]
eventos = [(correnTodos 30), (usarPowerUp (jetPack 3) azul), (usarPowerUp terremoto blanco), (correnTodos 40), (usarPowerUp (miguelitos 20) blanco), (usarPowerUp (jetPack 6) negro), (correnTodos 10)]
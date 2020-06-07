module Lib where
import Text.Show.Functions

laVerdad = True

{-Modelado del gimnasta
De cada gimnasta nos interesa saber su edad, su peso y su coeficiente de tonificación.-}

type Nombre = String
type Edad = Float
type Peso = Float
type CoeficienteDeTonificacion = Float

data Gimnasta = UnGimnasta {
    nombre :: String,
    edad :: Float,
    peso :: Float,
    coeficienteDeTonificacion :: Float
} deriving (Show, Eq)

pancho = UnGimnasta "Francisco" 40.0 120.0 1.0
andres = UnGimnasta "Andy" 22.0 80.0 6.0

{-Un ejemplo simple de ejercicio en el cual el gimnasta no hace nada (y por ende queda igual que al principio sin importar cuánto tiempo lo realice) podría ser: -}

relax ::  Float -> Gimnasta -> Gimnasta
relax minutos gimnasta = gimnasta


-- PUNTO 1 ---
{-Saber si alguien está saludable, lo cual se cumple si no está obeso y tiene una tonificación mayor a 5. Alguien es obeso si pesa más de 100 kilos. -}

saludable :: Gimnasta -> Bool
saludable gimnasta = (not . obeso) gimnasta && tonificado gimnasta

obeso :: Gimnasta -> Bool
obeso gimnasta = peso gimnasta > 100

tonificado :: Gimnasta -> Bool
tonificado gimanasta = coeficienteDeTonificacion gimanasta > 5


-- PUNTO 2 --
{-Hacer que el gimnasta queme una cantidad de calorías, lo que produce que baje de peso.-}
type Calorias = Float

quemarCalorias :: Gimnasta -> Calorias -> Gimnasta
quemarCalorias gimnasta calorias 
    | obeso gimnasta = perderPeso (calorias / 150) gimnasta
    | (not . obeso) gimnasta && edadMayorA 30 gimnasta && (calorias > 200 )= perderPeso 1 gimnasta
    | otherwise = perderPeso (calorias /(peso gimnasta * edad gimnasta)) gimnasta


perderPeso :: Peso -> Gimnasta -> Gimnasta
perderPeso pesoAperder gimnasta = gimnasta {peso = peso gimnasta - pesoAperder}

edadMayorA :: Float -> Gimnasta -> Bool
edadMayorA num gimnasta = edad gimnasta > num

-- PUNTO 3 -- 
{-La cinta quema calorías en función de la velocidad promedio alcanzada durante el ejercicio, quemando 1 caloría por la velocidad promedio por minuto.
La caminata es un ejercicio en cinta con velocidad constante de 5 km/h. -}
type Minutos = Float
type Inclinacion = Float
type Ejercicio = Minutos -> Gimnasta -> Gimnasta


caminataEnCinta :: Ejercicio
caminataEnCinta minutos gimnasta = quemarCalorias gimnasta (1 * velocidad * minutos)
    where velocidad = 5

{-El entrenamiento en cinta arranca en 6 km/h y cada 5 minutos incrementa la velocidad en 1 km/h, con lo cual la velocidad máxima dependerá de los minutos de entrenamiento.-}

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos gimnasta = quemarCalorias gimnasta (1 * (velocidadProm minutos) * minutos)

velocidadProm :: Minutos -> Float
velocidadProm = (/ 2).(+ 12).(/ 5)

{-Las pesas tonifican la décima parte de los kilos a levantar si se realiza por más de 10 minutos, sino nada.-}
type Pesas = Float

pesas :: Pesas -> Ejercicio
pesas kiloPesas minutos gimnasta = tonificarPorPesas (kiloPesas / 10) minutos gimnasta

tonificarPorPesas ::CoeficienteDeTonificacion -> Ejercicio
tonificarPorPesas coefTonificacion minutos gimnasta 
    | minutos > 10 = tonificar coefTonificacion gimnasta 
    | otherwise = gimnasta

tonificar ::  CoeficienteDeTonificacion -> Gimnasta -> Gimnasta
tonificar coefTonificacion gimnasta
    = gimnasta { coeficienteDeTonificacion = coeficienteDeTonificacion gimnasta + coefTonificacion}

{-La colina quema 2 calorías por minuto multiplicado por la inclinación de la colina. -}

colina :: Inclinacion -> Ejercicio
colina inclinacion minutos gimnasta = quemarCalorias gimnasta (2 * minutos * inclinacion)

{-La montaña son 2 colinas sucesivas (cada una con la mitad de duración respecto de los minutos totales indicados), donde la segunda colina tiene una inclinación de 3 más que la inclinación inicial elegida. 
Además de hacer perder peso por las calorías quemadas por las colinas, este ejercicio incrementa en una unidad la tonificación de la gimnasta. 
Resolver usando composición y aplicación parcial. -}

montania :: Inclinacion -> Ejercicio
montania inclinacionInicial minutos gimnasta = ((tonificar 1).(colina (inclinacionInicial + 3) min).(colina inclinacionInicial min)) gimnasta
    where min = minutos / 2

-- PUNTO 4 --
{-Rutina de ejercicios:
Dada una Rutina (es un Data con un nombre, duración total y lista de ejercicios específicos) y un gimnasta, obtener al gimnasta luego de realizar la rutina. La cantidad de minutos dedicada a cada ejercicio es la misma. 
Mostrar un ejemplo de uso usando todos los ejercicios del punto anterior. -}
type NombreRutina = String
type DuracionRutina = Float
type EjerciciosRutina = [Ejercicio]

data Rutina = UnaRutina {
    nombreRutina :: String,
    duracionTotal :: Float,
    ejercicios :: [Ejercicio]
}

rutina1 = UnaRutina {
    nombreRutina = "extrem",
    duracionTotal = 100,
    ejercicios = [caminataEnCinta, entrenamientoEnCinta, (pesas 50), (colina 10), (montania 10)]
}

-- **************************--
--    version 1
-- **************************--
{-Hacer otra solución usando fold.-}
hacerRutina :: Rutina -> Gimnasta -> Gimnasta
hacerRutina rutina gimnasta = foldl (aplicarRutina (duracionTotal rutina / fromIntegral cantDeEjercicios)) gimnasta (ejercicios rutina)
    where cantDeEjercicios = (length. ejercicios) rutina


aplicarRutina :: Minutos -> Gimnasta -> Ejercicio -> Gimnasta
aplicarRutina minutos gimnasta ejercicio = ejercicio minutos gimnasta

-- **************************--
--    Version 2
-- **************************--
{-Resolverlo usando recursividad.-}
hacerRutina' :: Rutina -> Gimnasta -> Gimnasta
hacerRutina' rutina gimnasta = evaluarRutina gimnasta (duracionTotal rutina / fromIntegral cantDeEjercicios) (ejercicios rutina)
    where cantDeEjercicios = (length. ejercicios) rutina

evaluarRutina :: Gimnasta -> Minutos -> [Ejercicio] -> Gimnasta
evaluarRutina gimnasta _ [] = gimnasta
evaluarRutina gimnasta minutos (ejercicio : ejercicios) = evaluarRutina (ejercicio minutos gimnasta) minutos ejercicios

-- **************************--
--    version 3
-- **************************--
hacerRutina'' :: Rutina -> Gimnasta -> Gimnasta
hacerRutina'' rutina gimnasta = (last . ejerciciosSucesivos gimnasta (duracionTotal rutina / fromIntegral cantDeEjercicios)) (ejercicios rutina)
    where cantDeEjercicios = (length. ejercicios) rutina

ejerciciosSucesivos :: Gimnasta -> Minutos -> [Ejercicio] -> [Gimnasta]
ejerciciosSucesivos gimnasta minutos ejercicios 
    = foldl (\gimnastaGenerado ejercicio -> gimnastaGenerado ++ [aplicarRutina minutos (last gimnastaGenerado) ejercicio]) [gimnasta] ejercicios

{-Dada una rutina y un gimnasta, obtener el resumen de rutina que es una tupla con el nombre de la misma, los kilos perdidos y la tonificación ganada 
por dicho gimnasta al realizarla. -}

resumenRutina :: Rutina -> Gimnasta -> (NombreRutina, Peso, CoeficienteDeTonificacion)
resumenRutina rutina gimnasta = (nombreRutina rutina, (peso nuevoGimnasta - peso gimnasta), (coeficienteDeTonificacion nuevoGimnasta - coeficienteDeTonificacion gimnasta))
    where nuevoGimnasta = hacerRutina' rutina gimnasta


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
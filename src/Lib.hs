module Lib where
import Text.Show.Functions

laVerdad = True

{-Modelado del gimnasta
De cada gimnasta nos interesa saber su edad, su peso y su coeficiente de tonificación.-}

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

Saludable :: Gimnasta -> Bool
estaSaludable gimnasta = (not . obeso) gimnasta && tonificado gimnasta

obeso :: Gimnasta -> Bool
obeso gimnasta = peso gimnasta > 100

tonificado :: Gimnasta -> Bool
tonificado gimanasta = coeficienteDeTonificacion gimanasta > 5
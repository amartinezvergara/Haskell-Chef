module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Ingrediente = Ingrediente{
    nombre::String,
    peso::Number
}deriving (Show, Eq)

data Plato = Plato{
    dificultad::Number,
    ingredientes::[Ingrediente]
}deriving (Show, Eq)

funcionazaPatternMatching :: Plato -> Bool
funcionazaPatternMatching unPlato{nombre = "Trollencio"} = True


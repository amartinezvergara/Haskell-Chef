module Library where
import PdePreludat
import Data.Foldable (find, Foldable (length, foldr))
import GHC.Stats (RTSStats(nonmoving_gc_elapsed_ns))

data Ingrediente = Ingrediente{
    nombre::String,
    peso::Number
}deriving (Show, Eq)

data Plato = Plato{
    dificultad::Number,
    ingredientes::[Ingrediente]
}deriving (Show, Eq)

data Participante = Participante{
    nombreParticipante :: String,
    especialidad :: Plato,
    truco :: (Plato->Plato) 
}deriving (Show)

hasIngrediente :: String -> Plato -> Bool
hasIngrediente nombreIngrediente plato = any(== nombreIngrediente).(map nombre) $ (ingredientes plato)

modificarPeso :: (Number->Number) -> Ingrediente -> Ingrediente
modificarPeso modificador ingrediente = ingrediente{peso= modificador (peso ingrediente)}

modificarPesoCriterioMap :: String -> (Number->Number) -> Ingrediente -> Ingrediente
modificarPesoCriterioMap nombreIng modificador ingrediente
    | nombreIng == nombre ingrediente = modificarPeso modificador ingrediente
    | otherwise = ingrediente

aniadirIngrediente :: Ingrediente -> Plato -> Plato
aniadirIngrediente ingrediente plato
    | hasIngrediente (nombre ingrediente) plato = plato{ingredientes = map (modificarPesoCriterioMap (nombre ingrediente) ((+).peso $ ingrediente)) (ingredientes plato)}
    | otherwise = plato{ingredientes = ingrediente : ingredientes plato}


endulzar :: Number -> Plato -> Plato
endulzar gramosAzucar unPlato = aniadirIngrediente (Ingrediente "azucar" gramosAzucar) unPlato

salar :: Number -> Plato -> Plato
salar gramosSal unPlato = aniadirIngrediente (Ingrediente "sal" gramosSal) unPlato

darSabor :: Number -> Number -> Plato -> Plato
darSabor gramosAzucar gramosSal unPlato = (endulzar gramosAzucar).(endulzar gramosAzucar) $ unPlato

duplicarPorcion :: Plato -> Plato
duplicarPorcion unPlato = unPlato{ingredientes = map(modificarPeso (*2)).ingredientes $ unPlato}

simplificar :: Plato -> Plato
simplificar unPlato
    | esComplejo unPlato = unPlato{ingredientes = filter((<= 10).peso) $ (ingredientes unPlato), dificultad = 5}
    | otherwise = unPlato

lacteos :: [String]
lacteos = ["leche", "queso", "yogur", "crema", "manteca", "dulce de leche"]

productosVeganos :: [String]
productosVeganos = lacteos ++ ["carne", "huevo"]

esVegano :: Plato -> Bool
esVegano unPlato = not.any (== True). map (flip hasIngrediente unPlato) $ productosVeganos

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not $ hasIngrediente "harina" unPlato

esComplejo :: Plato -> Bool
esComplejo unPlato = (PdePreludat.length.ingredientes $ unPlato) >= 5 && dificultad unPlato >= 7

mapSal :: Ingrediente -> Bool
mapSal unIngrediente = nombre unIngrediente == "sal" && peso unIngrediente > 2

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = any (==True) (map (mapSal) (ingredientes unPlato)) 

pepeRonccino :: Participante
pepeRonccino = Participante "Pepe Ronccino" laMejorPizzaDelUniverso ((duplicarPorcion).(simplificar).(darSabor 5 2))

laMejorPizzaDelUniverso :: Plato
laMejorPizzaDelUniverso = Plato 8 [Ingrediente "sal" 5, Ingrediente "harina" 500, Ingrediente "queso" 500, Ingrediente "salsa de tomate" 500, Ingrediente "oregano" 8, Ingrediente "pepperoni" 100] 

cocinar :: Participante -> Plato
cocinar unParticipante = truco unParticipante $ especialidad unParticipante

esMejorQue :: Plato -> Plato -> Bool
esMejorQue platoA platoB = dificultad platoA > dificultad platoB && sum (map (peso) $ ingredientes platoA) < sum (map(peso) $ ingredientes platoB)

participanteMejorPlato :: Participante -> Participante -> Participante
participanteMejorPlato part1 part2
    | esMejorQue (cocinar part1) (cocinar part2) = part1
    | otherwise = part2

participanteEstrella :: [Participante] -> Participante
participanteEstrella listaParticipantes = PdePreludat.foldr1 participanteMejorPlato $ listaParticipantes


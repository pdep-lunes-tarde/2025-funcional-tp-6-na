module Library where
import PdePreludat
import GHC.IO.Handle.Types (Handle__(haBufferMode))

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | BaconDeTofu | Papas | PatiVegano | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3
data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
    |tiene Carne hamburguesa = agregarIngrediente Carne hamburguesa
    |tiene PatiVegano hamburguesa = agregarIngrediente PatiVegano hamburguesa
    |otherwise = agregarIngrediente Pollo hamburguesa

tiene :: Ingrediente -> Hamburguesa -> Bool
tiene ingrediente hamburguesa = any (es ingrediente) (ingredientes hamburguesa)

es :: Ingrediente -> Ingrediente -> Bool
es ingredienteAComparar ingrediente = ingrediente == ingredienteAComparar

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa { ingredientes = ingrediente : ingredientes hamburguesa }

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa = hamburguesa {precioBase = precioBase hamburguesa - (precioBase hamburguesa * porcentaje / 100)}

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = descuento 30 . agregarIngrediente Papas $ hamburguesa

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = reemplazarSiTiene Carne PatiVegano . reemplazarSiTiene Pollo PatiVegano  . reemplazarSiTiene Cheddar QuesoDeAlmendras . reemplazarSiTiene Panceta BaconDeTofu $ hamburguesa

reemplazarSiTiene :: Ingrediente -> Ingrediente -> Hamburguesa -> Hamburguesa
reemplazarSiTiene ingredienteBuscado ingredienteNuevo hamburguesa
    |tiene ingredienteBuscado hamburguesa = reemplazarIngredientes ingredienteBuscado ingredienteNuevo $ hamburguesa
    |otherwise = hamburguesa

reemplazarIngredientes :: Ingrediente -> Ingrediente -> Hamburguesa -> Hamburguesa
reemplazarIngredientes ingredienteBuscado ingredienteNuevo hamburguesa = hamburguesa {ingredientes = map (esIgual ingredienteBuscado ingredienteNuevo) (ingredientes hamburguesa)}

esIgual :: Ingrediente -> Ingrediente -> Ingrediente -> Ingrediente
esIgual ingredienteBuscado ingredienteNuevo ingredienteDeLaLista
    |ingredienteBuscado == ingredienteDeLaLista = ingredienteNuevo
    |otherwise = ingredienteDeLaLista

calcularPrecioFinal :: Hamburguesa -> Number
calcularPrecioFinal hamburguesa = precioBase hamburguesa + (sum . map precioIngrediente $ ingredientes hamburguesa)

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa = reemplazarSiTiene Pan PanIntegral hamburguesa
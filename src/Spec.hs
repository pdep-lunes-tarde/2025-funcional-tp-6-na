module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

hamburguesaConCarne :: Hamburguesa
hamburguesaConCarne = Hamburguesa 20 [Pan,Carne]

hamburguesaConPollo :: Hamburguesa
hamburguesaConPollo = Hamburguesa 20 [Pan,Pollo]

hamburguesaConPolloYCarne :: Hamburguesa
hamburguesaConPolloYCarne = Hamburguesa 20 [Pan,Pollo,Carne]

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan,Carne,Cheddar,Pan]

pdepBurger :: Hamburguesa
pdepBurger = descuento 20 . agrandar . agrandar . agregarIngrediente Panceta . agregarIngrediente Cheddar $  cuartoDeLibra

dobleCuarto :: Hamburguesa
dobleCuarto = agrandar . agregarIngrediente Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = cambiarPanDePati . hacerVeggie $ dobleCuarto
correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 6" $ do
        describe "agrandar" $ do
            it "Dada una hamburguesa con carne, se le agrega carne" $ do
                agrandar hamburguesaConCarne `shouldBe` Hamburguesa 20 [Carne,Pan,Carne] 
            it "Dada una hamburguesa con pollo, se le agrega pollo" $ do
                agrandar hamburguesaConPollo `shouldBe` Hamburguesa 20 [Pollo,Pan,Pollo] 
            it "Dada una hambuerguesa con carne y pollo, se le agrega cualquiera" $ do
                agrandar hamburguesaConPolloYCarne `shouldBe` Hamburguesa 20 [Carne,Pan,Pollo,Carne] 
        describe "agregarIngrediente" $ do
            it "Dado un ingrediente y una hamburguesa, agrega el ingrediente a la hamburguesa" $ do
                agregarIngrediente Cheddar hamburguesaConCarne `shouldBe` Hamburguesa 20 [Cheddar,Pan,Carne]
        describe "descuento" $ do
            it "recibe un % de descuento, y devuelve la hamburguesa con ese descuento aplicado al precio base" $ do
                descuento 20 hamburguesaConCarne `shouldBe` Hamburguesa 16 [Pan,Carne]
        describe "Modelado de hamburguesas" $ do
            it "Dada una hamburguesa, la devuelve con los ingredientes esperados y precioFinal" $ do
                pdepBurger `shouldBe` Hamburguesa 16 [Carne,Carne,Panceta,Cheddar,Pan,Carne,Cheddar,Pan]
                dobleCuarto `shouldBe` Hamburguesa 20 [Carne,Cheddar,Pan,Carne,Cheddar,Pan]
                bigPdep `shouldBe` Hamburguesa 20 [Curry,Carne,Cheddar,Pan,Carne,Cheddar,Pan]
                dobleCuartoVegano `shouldBe` Hamburguesa 20 [PatiVegano,QuesoDeAlmendras,PanIntegral,PatiVegano,QuesoDeAlmendras,PanIntegral]
        describe "Precio final" $ do
            it "Dada una hamburguesa, devuelve la suma de los precios de todos sus ingredientes mas el precio base" $ do
                calcularPrecioFinal pdepBurger `shouldBe` 110
                calcularPrecioFinal dobleCuarto `shouldBe` 84
                calcularPrecioFinal bigPdep `shouldBe` 89
                (calcularPrecioFinal . delDia $ dobleCuarto) `shouldBe` 88
                (calcularPrecioFinal . hacerVeggie $ bigPdep) `shouldBe` 79
        describe "DelDia" $ do
            it "Dada una hamburguesa, le agrega papas y un descuento del 30%" $ do
                delDia bigPdep `shouldBe` Hamburguesa 14 [Papas,Curry,Carne,Cheddar,Pan,Carne,Cheddar,Pan]
        describe "hacerVeggie" $ do
            it "Dada una hamburguesa, cambia todos los ingredientes base que hayan en la hamburguesa por PatiVegano, el cheddar lo cambia por queso de almendras y la panceta por bacon de tofu." $ do
                hacerVeggie bigPdep  `shouldBe` Hamburguesa 20 [Curry,PatiVegano,QuesoDeAlmendras,Pan,PatiVegano,QuesoDeAlmendras,Pan]
        describe "cambiarPanDePati" $ do
            it "Dada una hamburguesa, se le cambia el pan por pan integral" $ do
                cambiarPanDePati hamburguesaConCarne `shouldBe` Hamburguesa 20 [PanIntegral,Carne]


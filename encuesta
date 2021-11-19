module Library1 where
import PdePreludat

data Pais = UnPais {
    nombre :: String ,
    poblacion :: Number , 
    indicadores :: [Indicador]
} deriving (Show, Eq)

data Indicador = UnIndicador {
    descripcion :: String ,
    valorNumerico :: Number 
} deriving (Show, Eq)

pais1 = UnPais "flandria" 4000 [indicador1,indicador2,indicador3,indicador4,indicador5]
pais2 = UnPais "x" 4000 [indicadorD,indicadorI]

indicador1 = UnIndicador "Desocupacion" 0.024
indicador2 = UnIndicador "Deuda Externa" 8000
indicador3 = UnIndicador "IVA" 0.21
indicador4 = UnIndicador "Reservas" 3000
indicador5 = UnIndicador "Indice Educativo" 100
indicadorD = UnIndicador "Deuda Externa" 1
indicadorI = UnIndicador "IVA" 0.21
--Punto 1

crecimientoVegetativo :: Pais -> Pais
crecimientoVegetativo pais = pais {poblacion = poblacion pais + poblacion pais * 5 / 100}

--Punto 2
esIgual ::  String -> Indicador  -> Bool
esIgual nombre indicador = descripcion indicador == nombre

buscaIndicador :: Pais -> String -> Indicador
buscaIndicador pais nombre = head(filter(esIgual(nombre)) (indicadores pais))

devuelveNumero :: Pais -> String -> Number
devuelveNumero pais nombre = valorNumerico (buscaIndicador pais nombre)


--Punto 2a
estaBien :: Pais -> Bool
estaBien pais = devuelveNumero pais "Deuda Externa"/ poblacion pais > devuelveNumero pais "IVA"
--Punto 2b
tieneFuturo ::  Number ->Pais  -> Bool
tieneFuturo valor pais = devuelveNumero pais "Indice Educativo" > valor

--Punto 2c
-- Va a ser potencia si la multiplicacion entre la deuda externa y el iva es menor a cantidad de 
-- habitantes multiplicado por desocupacion mas reservas mas indice educativo
esPotencia :: Pais -> Bool
esPotencia pais = (devuelveNumero pais "Deuda Externa")*(devuelveNumero pais "IVA") < (poblacion pais)*(devuelveNumero pais "Desocupacion") + (devuelveNumero pais "Reservas") + (devuelveNumero pais "Indice Educativo")

--Punto 2d
paises=[pais1,pais2,pais1]
conjuntosFuncion condicion (paises:otrosPaises)= filter condicion (paises:otrosPaises)

--Punto 3

data Fuerza = UnaFuerza {
    nombreFuerza :: String ,
    condicion :: (Pais->Bool),
    efecto :: (Pais->Pais)
}

cambioValores :: String -> (Number -> Number) -> Pais -> Pais
cambioValores nombre operacion  pais= pais {indicadores = (map (modificarValor operacion) (filter (esIgual nombre) (indicadores pais))) ++ (filter (not.(esIgual nombre)) (indicadores pais)) } 

modificarValor :: (Number -> Number) -> Indicador -> Indicador
modificarValor operacion indicador = indicador{valorNumerico = operacion(valorNumerico indicador)}

--Primera Fuerza Politica La hormiga ignorante

nocondicion :: p -> Bool
nocondicion pais = True

cambiaEducacion ::(Number->Number) -> Pais -> Pais
cambiaEducacion valor pais = cambioValores "Indice Educativo" valor pais



cambiaDesocupacion :: Pais -> Pais
cambiaDesocupacion  pais
    | (devuelveNumero pais "Desocupacion") > 10 = (cambioValores "Desocupacion" ((\numero -> numero - 1)) pais)
    | otherwise = pais

hormigaIgnorante = UnaFuerza "Hormiga Ignorante" nocondicion (cambiaDesocupacion.cambiaEducacion (*0.9))

--Segunda Fuerza Politica Eduqueitor

cambiaIva :: Pais -> Pais
cambiaIva pais = cambioValores "IVA" ((\numero -> numero * 0 + 24)) pais

eduqueitor = UnaFuerza "Eduqueitor" nocondicion (cambiaIva.cambiaEducacion(*1.4))

--Tercera Fuerza Duplicador
condicion1 pais =True
indicadorDuplica pais = (filter(condicion1)(indicadores pais))

duplicador = UnaFuerza "Duplicador" condicion1 (map(\x -> cambioValores (descripcion x) (*2) pais) (indicadorDuplica))

--Cuarta Fuerza CazaBuitre

cambiaDeuda :: Pais -> Pais
cambiaDeuda pais = cambioValores "Deuda Externa" (*0) pais

cazaBuitre = UnaFuerza "CazaBuitre" nocondicion cambiaDeuda

--Quinta fuerza, agrego una personal: Cierra muchas escuelas por lo que hace que el indice educativo baje
-- un valor de 50

cierraEscuelas = UnaFuerza "Cierra Escuela" nocondicion (cambiaEducacion ((\numero -> numero - 50)))

aplicarFuerzaAPais :: Fuerza -> Pais -> Pais
aplicarFuerzaAPais fuerza pais
    | (condicion fuerza) pais = (efecto fuerza) pais
    | otherwise = pais

--Punto 3a)
representaPeriodo :: Fuerza -> Pais -> Pais
representaPeriodo fuerza pais = (crecimientoVegetativo.(aplicarFuerzaAPais fuerza)) pais

--Punto 3b)
-- *Library1> representaPeriodo hormigaIgnorante pais1
--UnPais {nombre = "flandria", poblacion = 4200, indicadores = [UnIndicador {descripcion = "Indice Educativo", valorNumerico = 90},
--UnIndicador {descripcion = "Desocupacion", valorNumerico = 2.4e-2},UnIndicador {descripcion = "Deuda Externa", valorNumerico 
-- = 8000},UnIndicador {descripcion = "IVA", valorNumerico = 0.21},UnIndicador {descripcion = "Reservas", valorNumerico = 3000}]}

-- *Library1> representaPeriodo eduqueitor pais1      
--UnPais {nombre = "flandria", poblacion = 4200, indicadores = [UnIndicador {descripcion = "IVA", 
--valorNumerico = 24},UnIndicador {descripcion = "Indice Educativo", valorNumerico = 140},UnIndicador
--{descripcion = "Desocupacion", valorNumerico = 2.4e-2},UnIndicador {descripcion = "Deuda Externa",
--valorNumerico = 8000},UnIndicador {descripcion = "Reservas", valorNumerico = 3000}]}

-- *Library1> representaPeriodo cazaBuitre pais1
-- UnPais {nombre = "flandria", poblacion = 4200, indicadores = [UnIndicador {descripcion = "Deuda Externa", 
-- valorNumerico = 0},UnIndicador {descripcion = "Desocupacion", valorNumerico = 2.4e-2},UnIndicador
-- {descripcion = "IVA", valorNumerico = 0.21},UnIndicador {descripcion = "Reservas", valorNumerico = 3000},
-- UnIndicador {descripcion = "Indice Educativo", valorNumerico = 100}]}

-- *Library1> representaPeriodo cierraEscuelas pais1
-- UnPais {nombre = "flandria", poblacion = 4200, indicadores = [UnIndicador {descripcion = "Indice Educativo", valorNumerico = 50},
-- UnIndicador {descripcion = "Desocupacion", valorNumerico = 2.4e-2},UnIndicador {descripcion = "Deuda Externa", valorNumerico 
-- = 8000},UnIndicador {descripcion = "IVA", valorNumerico = 0.21},UnIndicador {descripcion = "Reservas", valorNumerico = 3000}]}

-- Punto 3c)


aplicarEfectoFuerza :: Pais -> Fuerza -> Pais
aplicarEfectoFuerza pais fuerza = (efecto fuerza) pais

aplicarFuerzas :: [Fuerza] -> Pais -> Pais
aplicarFuerzas listaFuerzas pais
     | (length listaFuerzas) > limiteReelecciones = aplicarFuerzas (take limiteReelecciones listaFuerzas) pais
     | otherwise = foldl aplicarEfectoFuerza pais listaFuerzas

limiteReelecciones = 3

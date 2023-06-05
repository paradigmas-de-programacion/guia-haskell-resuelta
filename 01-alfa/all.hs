-- Dado dos números enteros A y B, implemente una función que retorne la división entera de ambos por el método de las restas sucesivas
-- Ejemplo: divEntera 10 3 = 3
--          divEntera 10 2 = 5

divEntera :: Int -> Int -> Int
divEntera a b = if a < b
  then 0
  else 1 + divEntera (a-b) b

-- Escribir una función para hallar la potencia de un número
-- Ejemplo: potencia 2 3 = 8
--          potencia 3 2 = 9

potencia :: Int -> Int -> Int
potencia a 0 = 1
potencia a b = a * potencia a (b-1)

-- Definir una función menor que devuelve el menor de sus dos argumentos enteros
-- Ejemplo: menor 3 4 = 3
--          menor 5 2 = 2

menor :: Int -> Int -> Int
menor a b = if a < b
  then a
  else b

-- Definir una función maximoDeTres que devuelve el máximo de sus argumentos enteros
-- Ejemplo: maximoDeTres 6 2 3 = 6
--          maximoDeTres 5 8 1 = 8
--          maximoDeTres 3 4 5 = 5

mayor :: Int -> Int -> Int
mayor a b = if a < b
  then b
  else a

maximoDeTres :: Int -> Int -> Int -> Int
maximoDeTres a b c = mayor (mayor a b) c

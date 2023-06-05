-- Escribir una función que sume dos números enteros.
-- Ejemplo: suma 3 4 = 7
--          suma 5 2 = 7

suma :: Int -> Int -> Int
suma a b = a + b

-- Implementar una función que calcule el área de un círculo dado su radio.
-- Ejemplo: areaCirculo 3 = 28.274333882308138
--          areaCirculo 5 = 78.53981633974483

areaCirculo :: Float -> Float
areaCirculo r = pi * r * r

-- Definir una función que determine si un número es par o impar
-- Ejemplo: esPar 4 = True
--          esPar 5 = False

esPar :: Int -> Bool
esPar n = mod n 2 == 0

-- Implemente una función recursiva para calcular el factorial de un número
-- Ejemplo: factorial 5 = 120
--          factorial 3 = 6

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Implementar una función que invierta una lista
-- Ejemplo: invertir [1,2,3] = [3,2,1]
--          invertir [5,4,3,2,1] = [1,2,3,4,5]

invertir :: [a] -> [a]
invertir [] = []
invertir (x:xs) = invertir xs ++ [x]

-- Definir una función que determine si una lista está ordenada de forma ascendente
-- Ejemplo: ordenada [1,2,3] = True
--          ordenada [5,4,3,2,1] = False

ordenada :: [Int] -> Bool
ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs) = x <= y && ordenada (y:xs)

-- Escribir una función que cuente la cantidad de elementos en una lista.
-- Ejemplo: cantidad [1,2,3] = 3
--          cantidad [5,4,3,2,1] = 5

cantidad :: [a] -> Int
cantidad [] = 0
cantidad (x:xs) = 1 + cantidad xs

-- Implementar una función que obtenga los elementos en posiciones pares de una lista.
-- Ejemplo: pares [1,2,3,4,5] = [1,3,5]
--          pares [5,4,3,2,1] = [5,3,1]

pares :: [a] -> [a]
pares [] = []
pares [x] = [x]
pares (x:y:xs) = x : pares xs

-- Definir una función que calcule el máximo común divisor de dos números.
-- Ejemplo: mcd 10 5 = 5
--          mcd 5 3 = 1

mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (mod a b)

-- Implementar una función que calcule la suma de los dígitos de un número entero.
-- Ejemplo: sumaDigitos 123 = 6
--          sumaDigitos 12345 = 15

sumaDigitos :: Int -> Int
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)

-- Definir una función que encuentre el elemento mínimo en una lista.
-- Ejemplo: minimo [1,2,3] = 1
--          minimo [5,4,3,2,1] = 1

menor :: Int -> Int -> Int
menor a b = if a < b
  then a
  else b

minimo :: [Int] -> Int
minimo [x] = x
minimo (x:xs) = menor x (minimo xs)

-- Escribir una función que obtenga el enésimo número de la secuencia de Fibonacci.
-- Ejemplo: fibonacci 0 = 0
--          fibonacci 1 = 1
--          fibonacci 5 = 5

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

-- Implementar una función que verifique si una cadena de texto es un palíndromo.
-- Ejemplo: esPalindromo "anita lava la tina" = True
--          esPalindromo "hola mundo" = False

esPalindromo :: String -> Bool
esPalindromo s = s == reverse s

esPalindromo2 :: String -> Bool
esPalindromo2 [] = True
esPalindromo2 [x] = True
esPalindromo2 (x:xs) = x == last xs && esPalindromo2 (init xs)

-- Definir una función que elimine los duplicados de una lista.
-- Ejemplo: eliminarDuplicados [1,2,3,1,2] = [1,2,3]
--          eliminarDuplicados [5,4,3,2,1] = [5,4,3,2,1]

eliminarDuplicados :: [Int] -> [Int]
eliminarDuplicados [] = []
eliminarDuplicados [x] = [x]
eliminarDuplicados (x:xs) = if elem x xs
  then eliminarDuplicados xs
  else x : eliminarDuplicados xs

-- Implementar una función que obtenga el producto de todos los elementos de una lista.
-- Ejemplo: producto [1,2,3] = 6
--          producto [5,4,3,2,1] = 120

producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs

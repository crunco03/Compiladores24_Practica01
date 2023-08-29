-- Javier Alejandro Rivera Zavala
-- 28 de agosto de 2023
-- versión 1.0

module Ejercicio1y2 where
import Data.List (sort, nub)

---------------------------------------------------------------------------------------------------------------------

-- Función auxiliar para la función groupAnagrams.
-- Esta función verifica que 2 cadenas tengan los mismos caracteres aunque estén en distinto orden.
mismosCaracteres :: String -> String -> Bool
mismosCaracteres cadena1 cadena2 = sort cadena1 == sort cadena2

-- Ejercicio 1 - práctica 1.
-- Función que agrupa todas aquellas cadenas que son anagramas dada una lista de cadenas.
groupAnagrams :: [String] -> [[String]]
groupAnagrams listaCadenas =
  nub conjuntosCaracteres
  where
      conjuntosCaracteres = map snd [(str, filter (mismosCaracteres str) listaCadenas) | str <- listaCadenas]
   
---------------------------------------------------------------------------------------------------------------------

-- Ejercicio 2 - práctica 1.
-- Función que obtiene todas las posibles sublistas de una lista pasada como parámetro,
-- las listas representan conjuntos de elementos de un mismo tipo
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

  



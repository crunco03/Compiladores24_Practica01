module Practica01 where
import Data.List (sort, nub)
import Data.Map (Map, empty, insertWith, elems)

--1. Define la función groupAnagrams tal que recibe una lista de String y devuelve una lista con los anagramas agrupados. 
--Un anagrama es una palabra o frase formada al reorganizar las letras de otra palabra o frase, utilizando todas las letras originales exactamente una vez.

--groupAnagrams :: [ String ] -> [[ String ]]
    -- Ejemplo -
    -- > groupAnagrams [ " eat " ," tea " ," tan " ," ate " ," nat " ," bat " ]
    -- > [[ " eat " ," tea " ," ate " ] ,[ " tan " ," nat " ] ,[ " bat " ]]
    -- > groupAnagrams [ " hello " ," " ," world " ," wldro " ," hlloe " ," a " ," aa " ]
    -- > [[ " hello " ," hlloe " ] ,[ " " ] ,[ " world " ," wldro " ] ,[ " a " ] ,[ " aa " ]]

-- Función auxiliar para la función groupAnagrams.
-- Esta función verifica que 2 cadenas tengan los mismos caracteres aunque estén en distinto orden.

mismosCaracteres :: String -> String -> Bool
mismosCaracteres cadena1 cadena2 = sort cadena1 == sort cadena2

-- Función que agrupa todas aquellas cadenas que son anagramas dada una lista de cadenas.
groupAnagrams :: [String] -> [[String]]
groupAnagrams listaCadenas =
    elems $ foldr (\cadena -> insertWith (++) (sort cadena) [cadena]) empty listaCadenas


-- 2.Define la función subsets tal que recibe una lista de elementos únicos y devuelve el conjunto potencia.

--subsets :: [ a ] -> [[ a ]]
    --{ - Ejemplo -}
    -- > subsets [1 ,2 ,3]
    -- > [[] ,[3] ,[2] ,[2 ,3] ,[1] ,[1 ,3] ,[1 ,2] ,[1 ,2 ,3]]
    -- > subsets [ ’a ’ , ’b ’ , ’c ’ , ’d ’]
    -- > [ " " ," d " ," c " ," cd " ," b " ," bd " ," bc " ," bcd " ," a " ," ad " ," ac " ," acd " ," ab " ," abd " ," abc " ," abcd " ]
-- Función que obtiene todas las posibles sublistas de una lista pasada como parámetro,
-- las listas representan conjuntos de elementos de un mismo tipo
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

--3. El elemento mayoritario es el elemento que aparece más de ⌊n/2⌋ veces, donde n es la longitud de la lista. 
--Define la función majorityElem tal que recibe una lista y devuelve su elemento mayoritario.
--La solución debe ser de complejidad O(n) en tiempo y O(1) en el espacio.

--majorityElem :: Eq a => [ a ] -> a
    --{ - Ejemplo -}
    -- > majorityElem [3 ,2 ,3]
    -- > 3
    -- > majorityElem [2 ,2 ,1 ,1 ,1 ,2 ,2]
    -- > 2


--4. Define la función coins tal que recibe una lista de monedas de diferentes denominaciones y una cantidad total de dinero, 
--y devuelve si es posible completar la cantidad usando únicamente ese tipo de monedas.   

--coins :: [Int] -> Int -> Bool
    -- { - Ejemplo -}
    -- > coins [2 ,5] 8
    -- > True
    -- > coins [2 ,4 ,6] 21
    -- > False

coins :: [Int] -> Int -> Bool
coins _ 0 = True  -- Si la cantidad total es 0, siempre es posible completarla.
coins [] _ = False  -- Si la lista de monedas está vacía y la cantidad no es 0, no es posible.
coins (coin:rest) amount
    | amount < 0 = False  -- Si la cantidad se vuelve negativa, no es posible.
    | otherwise = coins rest amount || coins (coin:rest) (amount - coin)


---5. Considera la siguiente definición de árbol binario:
data BST a = Empty | Node a ( BST a ) ( BST a ) deriving Show

--Define la función isBST tal que recibe un árbol binario y devuelve si es un árbol de búsqueda binario válido. Un
--BST válido se define de la siguiente manera:
--(a) El subárbol izquierdo contiene solo valores menores que la raíz.
--(b) El subárbol derecho contiene solo valores mayores que la raíz.
--(c) Ambos subárboles deben ser árboles de búsqueda binarios.

maxNodeValue :: BST Int -> Int
maxNodeValue (Node v Empty Empty) = v
maxNodeValue (Node v leftSubTree Empty) = max (maxNodeValue leftSubTree) v
maxNodeValue (Node v Empty rightSubTree) = max v (maxNodeValue rightSubTree)
maxNodeValue (Node v leftSubTree rightSubTree) = maximum [maxNodeValue leftSubTree, v, maxNodeValue rightSubTree]

minNodeValue :: BST Int -> Int
minNodeValue (Node v Empty Empty) = v
minNodeValue (Node v leftSubTree Empty) = min (minNodeValue leftSubTree) v
minNodeValue (Node v Empty rightSubTree) = min v (minNodeValue rightSubTree)
minNodeValue (Node v leftSubTree rightSubTree) = minimum [minNodeValue leftSubTree, v, minNodeValue rightSubTree]

isBST :: BST Int -> Bool
    --{ - Ejemplo -}
    -- > isBST ( Node 3 ( Node 1 Empty ( Node 2 Empty Empty ) ) ( Node 4 Empty Empty ) )
    -- > True
    -- > isBST ( Node 3 ( Node 1 Empty ( Node 3 Empty Empty ) ) ( Node 4 Empty Empty ) )
    -- > False
isBST Empty = True
isBST (Node _ Empty Empty) = True
isBST (Node v leftSubTree Empty) = (maxNodeValue leftSubTree < v) && isBST leftSubTree
isBST (Node v Empty rightSubTree) = (v < minNodeValue rightSubTree) && isBST rightSubTree
isBST (Node v leftSubTree rightSubTree) = (maxNodeValue leftSubTree < v) && (v < minNodeValue rightSubTree) && isBST leftSubTree && isBST rightSubTree



--6. Define la función kthElem tal que recibe un árbol de búsqueda binaria y un número entero k, y devuelve el k-ésimo valor más pequeño.

--kthElem :: BST a -> Int -> a
    --{ - Ejemplo -}
    -- > kthElem ( Node 3 ( Node 1 Empty ( Node 2 Empty Empty ) ) ( Node 4 Empty Empty ) ) 2
    -- > 2
    -- > kthElem ( Node 5 ( Node 3 ( Node 2 ( Node 1 Empty Empty ) Empty ) ( Node 4 Empty Empty ) ) ( Node 6 Empty Empty ) ) 4
    -- > 4


kthElem :: BST a -> Int -> a
kthElem Empty _ = error "Árbol vacío"
kthElem (Node v leftSubTree rightSubTree) k
  | k <= size leftSubTree = kthElem leftSubTree k
  | k == size leftSubTree + 1 = v
  | otherwise = kthElem rightSubTree (k - size leftSubTree - 1)
  where
    size Empty = 0
    size (Node _ left right) = 1 + size left + size right



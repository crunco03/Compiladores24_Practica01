--1. Define la función groupAnagrams tal que recibe una lista de String y devuelve una lista con los anagramas agrupados. 
--Un anagrama es una palabra o frase formada al reorganizar las letras de otra palabra o frase, utilizando todas las letras originales exactamente una vez.

groupAnagrams :: [ String ] -> [[ String ]]
    -- Ejemplo -
    -- > groupAnagrams [ " eat " ," tea " ," tan " ," ate " ," nat " ," bat " ]
    -- > [[ " eat " ," tea " ," ate " ] ,[ " tan " ," nat " ] ,[ " bat " ]]
    -- > groupAnagrams [ " hello " ," " ," world " ," wldro " ," hlloe " ," a " ," aa " ]
    -- > [[ " hello " ," hlloe " ] ,[ " " ] ,[ " world " ," wldro " ] ,[ " a " ] ,[ " aa " ]]


-- 2.Define la función subsets tal que recibe una lista de elementos únicos y devuelve el conjunto potencia.

subsets :: [ a ] -> [[ a ]]
    --{ - Ejemplo -}
    -- > subsets [1 ,2 ,3]
    -- > [[] ,[3] ,[2] ,[2 ,3] ,[1] ,[1 ,3] ,[1 ,2] ,[1 ,2 ,3]]
    -- > subsets [ ’a ’ , ’b ’ , ’c ’ , ’d ’]
    -- > [ " " ," d " ," c " ," cd " ," b " ," bd " ," bc " ," bcd " ," a " ," ad " ," ac " ," acd " ," ab " ," abd " ," abc " ," abcd " ]


--3. El elemento mayoritario es el elemento que aparece más de ⌊n/2⌋ veces, donde n es la longitud de la lista. 
--Define la función majorityElem tal que recibe una lista y devuelve su elemento mayoritario.
--La solución debe ser de complejidad O(n) en tiempo y O(1) en el espacio.

majorityElem :: Eq a = > [ a ] -> a
    --{ - Ejemplo -}
    -- > majorityElem [3 ,2 ,3]
    -- > 3
    -- > majorityElem [2 ,2 ,1 ,1 ,1 ,2 ,2]
    -- > 2


--4. Define la función coins tal que recibe una lista de monedas de diferentes denominaciones y una cantidad total de dinero, y devuelve si es posible completar la cantidad usando únicamente ese tipo de monedas.   

coins :: [Int] -> Int -> Bool
    -- { - Ejemplo -}
    -- > coins [2 ,5] 8
    -- > True
    -- > coins [2 ,4 ,6] 21
    -- > False



---5. Considera la siguiente definición de árbol binario:
data BST a = Empty | Node a ( BST a ) ( BST a ) deriving Show

--Define la función isBST tal que recibe un árbol binario y devuelve si es un árbol de búsqueda binario válido. Un
--BST válido se define de la siguiente manera:
--(a) El subárbol izquierdo contiene solo valores menores que la raíz.
--(b) El subárbol derecho contiene solo valores mayores que la raíz.
--(c) Ambos subárboles deben ser árboles de búsqueda binarios.


isBST :: BST Int -> Bool
    --{ - Ejemplo -}
    -- > isBST ( Node 3 ( Node 1 Empty ( Node 2 Empty Empty ) ) ( Node 4 Empty Empty ) )
    -- > True
    -- > isBST ( Node 3 ( Node 1 Empty ( Node 3 Empty Empty ) ) ( Node 4 Empty Empty ) )
    -- > False



--6. Define la función kthElem tal que recibe un árbol de búsqueda binaria y un número entero k, y devuelve el k-ésimo valor más pequeño.

kthElem :: BST a -> Int -> a
    --{ - Ejemplo -}
    -- > kthElem ( Node 3 ( Node 1 Empty ( Node 2 Empty Empty ) ) ( Node 4 Empty Empty ) ) 2
    -- > 2
    -- > kthElem ( Node 5 ( Node 3 ( Node 2 ( Node 1 Empty Empty ) Empty ) ( Node 4 Empty Empty ) )
    -- ( Node 6 Empty Empty ) ) 4
    -- > 4
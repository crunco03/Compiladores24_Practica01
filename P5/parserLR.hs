-----------------------------------------------------------------------------------------
-- Miembros del equipo:
-- Gabriela Cruz Blanco
-- Javier Alejandro Rivera Zavala
-- Ulises Rodríguez García
-- Zurisadai Uribe García
-- Sergio Vasconcelos Carranza
-----------------------------------------------------------------------------------------

-- Definiciones de datos
data Token = Assign | If | Then | Else | Seq | While | Do | Skip | Boolean Bool | Equal | And | Not | Loc Int | Number Int | LP | RP | Sum deriving Show 
data Content = T Token | S | C | PC | B | PB | E | PE deriving Show
data State = Q Int deriving Show

type Input = [Token]
type Stack = [State]
type Symbols = [Content]

-- Utilizando la función parserAux, define la función parser que recibe una lista de tokens WHILE y devuelve
-- verdadero si y solo si la lista de tokens pertenece al lenguaje.
parser :: Input -> Bool
parser tokens = parserAux tokens [Q 0] []


-- Define la función parserAux que recibe una lista de tokens, el stack de estados y símbolos, y devuelve verdadero si
-- y solo si la lista de tokens pertenece al lenguaje con los stacks actuales. Esta función es un parser LR(1) ad hoc del
-- lenguaje.
parserAux :: Input -> Stack -> Symbols -> Bool
--SHIFT
--Verificamos el primer token del input
--Verificamos el estado en que nos encontramos
--Continuamos la derivacion transicionando al agregar un nuevo estado y el simbolo que leimos
parserAux (Seq:xs) (Q 2:ys) ws = parserAux xs (Q 5:Q 2:ys) (T Seq:ws)

parserAux (Loc n:xs) (Q 0:ys) ws = parserAux xs (Q 3:Q 0:ys) (T (Loc n):ws)
parserAux (Loc n:xs) (Q 5:ys) ws = parserAux xs (Q 3:Q 5:ys) (T (Loc n):ws)
parserAux (Loc n:xs) (Q 6:ys) ws = parserAux xs (Q 10:Q 6:ys) (T (Loc n):ws)
parserAux (Loc n:xs) (Q 12:ys) ws = parserAux xs (Q 10:Q 12:ys) (T (Loc n):ws)

parserAux (Assign:xs) (Q 3:ys) ws = parserAux xs (Q 6:Q 3:ys) (T Assign:ws)

parserAux (Skip:xs) (Q 0:ys) ws = parserAux xs (Q 4:Q 0:ys) (T Skip:ws)
parserAux (Skip:xs) (Q 5:ys) ws = parserAux xs (Q 5:Q 4:ys) (T Skip:ws)

parserAux (Sum:xs) (Q 9:ys) ws = parserAux xs (Q 12:Q 9:ys) (T Sum:ws)

parserAux (Number n:xs) (Q 6:ys) ws = parserAux xs (Q 11:Q 6:ys) (T (Number n):ws)
parserAux (Number n:xs) (Q 12:ys) ws = parserAux xs (Q 11:Q 12:ys) (T (Number n):ws)

--REDUCE 
--Verificamos el primer token del input (el simbolo de fin de cadena es equivalente a un input vacio)
--Verificamos el estado en el que nos encontramos
--Verificamos que los simbolos y estados que vamos a descartar son correctos
--Continuamos la derivacion reemplazando estados y simbolos

parserAux [] (Q 4:ys) (T Skip:ws) = parserAux [] (Q 2:ys) (PC:ws)
parserAux [] (Q 2:ys) (PC:ws) = parserAux [] (Q 1:ys) (C:ws)


parserAux xs (Q 5:ys) ws = parserAux xs (Q 7:ys) (T Seq:ws)

parserAux xs (Q 5:ys) ws = parserAux xs (Q 7:ys) (T Skip:ws)

parserAux xs (Q 12:ys) ws = parserAux xs (Q 7:ys) (T Sum:ws)

parserAux xs (Q 10:ys) (T (Loc n):ws) = parserAux xs (Q 7:ys) (T (Loc n):ws)


--ACCEPT
parserAux [] (Q 1:ys) ws = True

--REJECT
parserAux _ _ _ = False


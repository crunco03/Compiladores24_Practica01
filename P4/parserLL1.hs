-- Definiciones de datos
data Token = Assign | If | Then | Else | Seq | While | Do | Skip | Boolean Bool | Equal | And | Not | Loc Int | Number Int | LP | RP | Sum deriving Show
data Content = T Token | S | C | B | E deriving Show
type Input = [Token]
type Stack = [Content]


parserAux :: Input -> Stack -> Bool
parserAux [] [] = True
parserAux (_:_) [] = False
parserAux [] (_:_) = False 

parserAux (Loc n:xs) (T (Loc m):ys) = if n == m then parserAux xs ys else False
parserAux (Loc n:xs) (S:ys) = parserAux (Loc n:xs) (C:ys)
parserAux (Loc n:xs) (C:ys) = parserAux (Loc n:xs) (T (Loc n):T Equal:E:ys)
parserAux (Loc n:xs) (B:ys) = parserAux (Loc n:xs) (E:T Equal:E:ys)
parserAux (Loc n:xs) (E:ys) = parserAux (Loc n:xs) (T (Loc n):ys)

parserAux (Number n:xs) (T (Number m):ys) = if n == m then parserAux xs ys else False
parserAux (Number n:xs) (B:ys) = parserAux (Number n:xs) (E:T Equal:E:ys)
parserAux (Number n:xs) (E:ys) = parserAux (Number n:xs) (T (Number n):ys)

parserAux (If:xs) (T If:ys) = parserAux xs ys
parserAux (If:xs) (S:ys) = parserAux (If:xs) (C:ys)
parserAux (If:xs) (C:ys) = parserAux (If:xs) (T If:B:T Then:C:T Else:C:ys)

parserAux (LP:xs) (T LP:ys) = parserAux xs ys
parserAux (LP:xs) (S:ys) = parserAux (LP:xs) (C:ys)
parserAux (LP:xs) (C:ys) = parserAux (LP:xs) (T LP:C:T Seq:C:T RP:ys)
parserAux (LP:xs) (B:ys) = parserAux (LP:xs) (E:T Equal:E:ys)
parserAux (LP:xs) (E:ys) = parserAux (LP:xs) (T LP:E:T Sum:E:T RP:ys)

parserAux (While:xs) (T While:ys) = parserAux xs ys
parserAux (While:xs) (S:ys) = parserAux (While:xs) (C:ys)
parserAux (While:xs) (C:ys) = parserAux (While:xs) (T While:B:T Do:C:ys)

parserAux (Skip:xs) (T Skip:ys) =  parserAux xs ys
parserAux (Skip:xs) (S:ys) =  parserAux (Skip:xs) (C:ys)
parserAux (Skip:xs) (C:ys) = parserAux (Skip:xs) (T Skip:ys)

parserAux (Boolean True:xs) (T (Boolean True):ys) = parserAux xs ys
parserAux (Boolean True:xs) (B:ys) = parserAux (Boolean True:xs) (T (Boolean True):ys)

parserAux (Boolean False:xs) (T (Boolean False):ys) = parserAux xs ys
parserAux (Boolean False:xs) (B:ys) = parserAux (Boolean False:xs) (T (Boolean False):ys)

parserAux (And:xs) (T And:ys) = parserAux xs ys
parserAux (And:xs) (B:ys) = parserAux (And:xs) (T And:B:B:ys)

parserAux (Not:xs) (T Not:ys) = parserAux xs ys
parserAux (Not:xs) (B:ys) = parserAux (Not:xs) (T Not:B:ys)

parserAux (Sum:xs) (T Sum:ys) = parserAux xs ys
parserAux (Then:xs) (T Then:ys) = parserAux xs ys
parserAux (Else:xs) (T Else:ys) = parserAux xs ys
parserAux (Seq:xs) (T Seq:ys) = parserAux xs ys
parserAux (RP:xs) (T RP:ys) = parserAux xs ys
parserAux (Do:xs) (T Do:ys) = parserAux xs ys
parserAux (Equal:xs) (T Equal:ys) = parserAux xs ys
parserAux (Assign:xs) (T Assign:ys) = parserAux xs ys
parserAux (x:xs) _ = error "Unexpected Token"


          

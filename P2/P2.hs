module P2 where
import Data.Char(isAlpha,isDigit,isSpace)



---------------------------------- Análisis léxico ----------------------------------

data Token = Var String | Number Int | Boolean Bool | Sum | Subs | And | Or | Equal 
            deriving Show
            
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexString (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('+':cs) = Sum : lexer cs
lexer ('-':cs) = Subs : lexer cs
lexer ('&':'&':rest) = And : lexer rest
lexer ('|':'|':rest) = Or : lexer rest
lexer ('=':'=':cs) = Equal : lexer cs

lexNum cs = Number (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexString cs =
   case span isAlpha cs of
      ("t", rest) -> Boolean (True) : lexer rest
      ("f", rest) -> Boolean (False) : lexer rest
      (var,rest)   -> Var (var) : lexer rest
      
      
      
---------------------------------- Análisis sintáctico ----------------------------------

data ASA = VarASA String | NumberASA Int | BooleanASA Bool | Op Token ASA ASA deriving Show
type Stack = [ASA]

scanner :: [Token] -> ASA
scanner tokens = scannerAux tokens []



scannerAux :: [Token] -> Stack -> ASA
scannerAux [] (asa:_) = asa
scannerAux (token:tokens) pila
    | esOperando token = scannerAux tokens (convertirTokenASA token : pila)
    | esOperador token = case pila of
        (derecha : izquierda : pilaRestante) -> scannerAux tokens (Op token derecha izquierda : pilaRestante)
        _ -> error "Expresión mal formada"
    | otherwise = error "Token desconocido"

esOperando :: Token -> Bool
esOperando (Var _) = True
esOperando (Number _) = True
esOperando (Boolean _) = True
esOperando _ = False

esOperador :: Token -> Bool
esOperador Sum = True
esOperador Subs = True
esOperador And = True
esOperador Or = True
esOperador Equal = True
esOperador _ = False

convertirTokenASA :: Token -> ASA
convertirTokenASA (Var x) = VarASA x
convertirTokenASA (Number n) = NumberASA n
convertirTokenASA (Boolean b) = BooleanASA b
convertirTokenASA _ = error "Token desconocido"

      
      
      
---------------------------------- Análisis semántico ----------------------------------

data Type = Num | Bool deriving Show

--typeCheckerAux :: ASA -> Type

--typeChecker :: ASA -> ASA
      
      
      
---------------------------------- Optimización de Código Fuente ----------------------------------

--constantFolding :: ASA -> ASA


data Value = N Int | B Bool | S String
instance Show Value where
        show (N n) = show n
        show (B b) = show b
        show (S s) = show s

tokenThreeAddress :: Token -> String
tokenThreeAddress Sum = "+"
tokenThreeAddress Subs = "-"
tokenThreeAddress And = "&&"
tokenThreeAddress Or = "||"
tokenThreeAddress Equal = "=="

data ThreeAddress = Assign String Value | Operation String String Token String
instance Show ThreeAddress where
        show (Assign t v) = show t ++ " = " ++ show v
        show (Operation t a op b) = show t ++ " = " ++ show a ++ tokenThreeAddress op ++ show b

fresh :: [Int] -> Int
fresh ts = head [n | n <- [0..], n `notElem` ts]

threeAddressAux :: ASA -> [Int] -> ([ThreeAddress],String,[Int])
threeAddressAux (VarASA v) ts = (c',temp,i:ts)
                                    where 
                                        i = fresh ts
                                        temp = "t" ++ show i
                                        c' = [Assign temp (S v)]
threeAddressAux (NumberASA n) ts = (c',temp,i:ts)
                                    where 
                                        i = fresh ts
                                        temp = "t" ++ show i
                                        c' = [Assign temp (N n)]
threeAddressAux (BooleanASA b) ts = (c',temp,i:ts)
                                    where 
                                        i = fresh ts
                                        temp = "t" ++ show i
                                        c' = [Assign temp (B b)]
threeAddressAux (Op op a b) ts = (a' ++ b' ++ c',temp,i:bs)
                                    where 
                                        (a',adA,as) = threeAddressAux a ts
                                        (b',adB,bs) = threeAddressAux b as
                                        i = fresh bs
                                        temp = "t" ++ show i
                                        c' = [Operation temp adA op adB]

threeAddress :: ASA -> [ThreeAddress]
threeAddress asa = trad
                    where 
                    (trad,temp,ts) = threeAddressAux asa []


---------------------------------- Generación de Código ----------------------------------

--assembly :: [ ThreeAddress ] -> String



---------------------------------- Extra ----------------------------------

--compile :: String -> String

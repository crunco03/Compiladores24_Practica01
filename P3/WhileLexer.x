{
module WhileLexer where
}

%wrapper "basic"

$digit = 0-9

tokens :-

    $white+      ;
    "--".*       ;
    
    "if"         { \s -> If }
    "then"       { \s -> Then }
    "else"       { \s -> Else }
    "while"      { \s -> While }
    "do"         { \s -> Do }
    "skip"       { \s -> Skip }
    "true"       { \s -> Boolean True }
    "false"      { \s -> Boolean False }
    ":="         { \s -> Assign }
    ";"          { \s -> Seq }
    "="          { \s -> Equal }
    "&"          { \s -> And }
    "-"          { \s -> Not }
    "("          { \s -> LP }
    ")"          { \s -> RP }
    "+"          { \s -> Sum }
    "L"$digit+   { \s -> Loc (read (tail s)) }
    $digit+      { \s -> Number (read s) }

{

data Token = Assign | If | Then | Else | Seq | While | Do | Skip |
             Boolean Bool | Equal | And | Not |
             Loc Int | Number Int | LP | RP | Sum deriving Show

lexer :: String -> [Token]
lexer = alexScanTokens

}

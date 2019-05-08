{
module Tokens where
}

%wrapper "basic"

$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  "1"                            { \s -> TokenOne }
  "*"                            { \s -> TokenProduct }
  "+"                            { \s -> TokenSum }
  "("                            { \s -> TokenLparen }
  ")"                            { \s -> TokenRparen }  
  ":"                            { \s -> TokenColon }
  "|-"                           { \s -> TokenTurnstile }  
  "iden"                         { \s -> TokenIden}
  "comp"                         { \s -> TokenComp}
  "unit"                         { \s -> TokenUnit}  

{

-- The token type:
data Token = TokenOne
           | TokenProduct
           | TokenSum
           | TokenColon           
           | TokenLparen
           | TokenRparen
           | TokenIden
           | TokenComp
           | TokenUnit
           | TokenTurnstile
           deriving (Eq,Show)

scanTokens = alexScanTokens

}

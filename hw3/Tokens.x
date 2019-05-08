{
module Tokens where
}

%wrapper "basic"

$alpha = [a-zA-Z]
$numbers = [0-9]

tokens :-

  $white+                       ;
  "--".*                        ;
  $alpha+$numbers*               { \s -> TokenVar s }
  "("                            { \s -> TokenLParen }
  ")"                            { \s -> TokenRParen }

{

-- The token type:
data Token = TokenVar String
           | TokenLParen
           | TokenRParen          
           deriving (Eq,Show)

scanTokens = alexScanTokens

}

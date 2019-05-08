{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  "let"                         { \s -> TokenLet }
  "in"                            { \s -> TokenIn }
  "="                            { \s -> TokenEq }
  "+"                          { \s -> TokenPlus }
  "*"                            { \s -> TokenTimes }
  "("                            { \s -> TokenLParen }
  ")"                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }
  $digit+                       { \s -> TokenNum (read s :: Integer) }  

{

-- The token type:
data Token = TokenLet
           | TokenIn
           | TokenSym String
           | TokenNum Integer           
           | TokenEq
           | TokenPlus
           | TokenTimes
           | TokenLParen
           | TokenRParen
           deriving (Eq,Show)

scanTokens = alexScanTokens

}

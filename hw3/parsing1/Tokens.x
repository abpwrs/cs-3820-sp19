{
module Tokens where
}

%wrapper "basic"

$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  "@"                            { \s -> TokenAt }
  "$"                            { \s -> TokenDollar }
  $alpha+ { \s -> TokenSym s }

{

-- The token type:
data Token = TokenAt
           | TokenDollar
           | TokenSym String
           deriving (Eq,Show)

scanTokens = alexScanTokens

}

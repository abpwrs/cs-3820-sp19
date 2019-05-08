{
module Grammar where
import Tokens
import Expr
}

%name parseExpr
%tokentype { Token }
%error { parseError }

%token
    var { TokenVar $$ }
    '(' { TokenLParen }
    ')' { TokenRParen }

%%

Exp :   var              { Value $1 }
    |  '(' var List ')'  { Head $2 $3 }

List : Exp              { [$1] }
     | Exp List         { $1 : $2 }

{
parseError :: [Token] -> a
parseError tks = error ("Parse error: " ++ show tks)

}

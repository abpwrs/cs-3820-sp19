{
module Grammar where
import Tokens
import Expr
}

%name parseExpr
%tokentype { Token }
%error { parseError }

%token
    var { TokenSym $$ }
    '@' { TokenAt }
    '$' { TokenDollar }

%%

Exp :
  Exp var '$'           { B $1 $2 }
| Exp Exp '@'           { A $1 $2 }
| var                   { V $1 }

{

parseError :: [Token] -> a
parseError tks = error ("Parse error: " ++ show tks)

}

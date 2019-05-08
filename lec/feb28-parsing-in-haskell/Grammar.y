{
module Grammar where
import Tokens
import Expr
}

%name parseExpr
%tokentype { Token }
%error { parseError }

%token
    let { TokenLet }
    in  { TokenIn }
    '=' { TokenEq }
     var { TokenSym $$ }
    num { TokenNum $$ }
    '+' { TokenPlus }
    '*' { TokenTimes }
    '(' { TokenLParen }
    ')' { TokenRParen }

%right in
%right '+'
%right '*' 

%%

Exp :
  let var '=' Exp in Exp { Let $2 $4 $6 }
| Exp '+' Exp            { Add $1 $3 }
| Exp '*' Exp            { Mult $1 $3 }
| '(' Exp ')'            { $2 }
| num                  { Num $1 }
| var                    { Var $1 }

{

parseError :: [Token] -> a
parseError tks = error ("Parse error: " ++ show tks)

}

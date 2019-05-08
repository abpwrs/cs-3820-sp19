{
module Grammar where
import Tokens
import Expr
}

%name parseTyping
%tokentype { Token }
%error { parseError }

%token
    '1' { TokenOne }
    '*' { TokenProduct }
    '+' { TokenSum }
    '(' { TokenLparen }
    ')' { TokenRparen }
    ':' { TokenColon }
    '|-' { TokenTurnstile }
    'iden' { TokenIden }
    'comp' { TokenComp }
    'unit' { TokenUnit }

%%

Typing :
  Term ':' Type '|-' Type { Typing $1 $3 $5 }

Type :
  '1' { One }
| '(' Type '*' Type ')' { Product $2 $4 }
| '(' Type '+' Type ')' { Sum $2 $4 }

Term :
  'iden' { Iden }
| 'comp' Type {- type B -} Term Term { Comp $2 $3 $4 }
| 'unit' { Unit }

{

parseError :: [Token] -> a
parseError tks = error ("Parse error: " ++ show tks)

}

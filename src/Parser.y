{
module Parser (parseExpr, showAST, showASTTree, parse) where

import Lexer
import Expr
import Data.List
import Data.Tree
import Data.Tree.Pretty

}

%name parse
%tokentype { Token }
%error { parseError }

%token
    read        { READ _            }
    write       { WRITE _           }
    while       { WHILE _           }
    do          { DO _              }
    if          { IF _              }
    then        { THEN _            }
    else        { ELSE _            }
    func        { FUNCTION _        }
    return      { RETURN _          }
    num         { NUM _ $$ _        }
    var         { IDENT _ $$        }
    true        { TRUE _            }
    false       { FALSE _           }
    ':='        { ASSIGN _          }
    '+='        { PLUSASSIGN _      }
    '-='        { MINUSASSIGN _     }
    '*='        { MULTASSIGN _      }
    '/='        { DIVASSIGN _       }
    ';'         { SEMI _            }
    '+'         { OP _ Plus _       }
    '-'         { OP _ Minus _      }
    '*'         { OP _ Mult _       }
    '/'         { OP _ Div _        }
    '%'         { OP _ Mod _        }
    '**'        { OP _ Pow _        }
    '=='        { OP _ Eq _         }
    '!='        { OP _ Ne _         }
    '>'         { OP _ Gt _         }
    '>='        { OP _ Ge _         }
    '<'         { OP _ Lt _         }
    '<='        { OP _ Le _         }
    '&&'        { OP _ And _        }
    '||'        { OP _ Or _         }
    '('         { OPEN_PAR _        }
    ')'         { CLOSE_PAR _       }
    '{'         { OPEN_BRACE _      }
    '}'         { CLOSE_BRACE _     }
    ','         { COMMA _           }
    '?'         { QUESTION _        }
    ':'         { COLON _           }


%nonassoc '&&' '||'
%nonassoc '==' '!='
%nonassoc '>' '>=' '<' '<='
%left '('
%left '+' '-'
%left '*' '/' '%'
%right '**'
%left NEG
%%

Prog :  Decls Stmts                         { Prog $1 $2}


Decls :  Decls Decl                         { $1 ++ [$2] }
        | {- empty -}                       { [] }

Stmts : Stmts Stmt                          { $1 ++ [$2] }
        | {- empty -}                       { [] }



Decl :  func var '(' VArgs ')' '{' Stmts '}'   { Decl $2 $4 $7}


Stmt : var ':=' Expr ';'                    { Assign (Var $1) $3 }
     | write '(' Expr ')' ';'               { Write $3 }
     | read '(' Expr ')' ';'                { Read $3 }
     | while Expr do '{' Stmts '}'          { WhileLoop $2 $5 }
     | if Expr then '{' Stmts '}' else '{' Stmts '}'
                                            { IfCond $2 $5 $9 }
     | var '(' Args ')' ';'                 { FuncCall $1 $3}
     | return Expr ';'                      { Return $2 }
     | var ':=' '(' Expr ')' '?' Expr ':' Expr ';'
                                            { IfCond $4 [Assign (Var $1) $7] [Assign (Var $1) $9] }
     | '+''+' var ';'                       { Assign (Var $3) (BinOp Plus (Var $3) (Num 1.0)) }
     | var '+=' Expr ';'                    { Assign (Var $1) (BinOp Plus (Var $1) $3) }
     | var '-=' Expr ';'                    { Assign (Var $1) (BinOp Minus (Var $1) $3) }
     | var '*=' Expr ';'                    { Assign (Var $1) (BinOp Mult (Var $1) $3) }
     | var '/=' Expr ';'                    { Assign (Var $1) (BinOp Div (Var $1) $3) }


Expr :  true                                { TrueConst }
        | false                             { FalseConst }
        | var '(' Args ')'                  { FCall $1 $3}
        | num                               { Num $1 }
        | var                               { Var $1 }
        | Expr '+' Expr                     { BinOp Plus $1 $3 }
        | Expr '-' Expr                     { BinOp Minus $1 $3 }
        | '-' Expr     %prec NEG            { Unary Minus $2 }
        | Expr '/' Expr                     { BinOp Div $1 $3 }
        | Expr '%' Expr                     { BinOp Mod $1 $3 }
        | Expr '**' Expr                    { BinOp Pow $1 $3 }
        | Expr '*' Expr                     { BinOp Mult $1 $3 }
        | Expr '==' Expr                    { BinOp Eq $1 $3 }
        | Expr '!=' Expr                    { BinOp Ne $1 $3 }
        | Expr '>=' Expr                    { BinOp Ge $1 $3 }
        | Expr '>' Expr                     { BinOp Gt $1 $3 }
        | Expr '<=' Expr                    { BinOp Le $1 $3 }
        | Expr '<' Expr                     { BinOp Lt $1 $3 }
        | Expr '&&' Expr                    { BinOp And $1 $3 }
        | Expr '||' Expr                    { BinOp Or $1 $3 }
        | '(' Expr ')'                      { $2 }


Args : Args ',' Expr                        { $1 ++ [$3] }
       | Expr                                  { [$1] }
       | {- empty -}                           { [] }


VArgs : VArgs ',' var                      { $1 ++ [Var $3] }
        | var                               { [Var $1] }
        | {- empty -}                       { [] }


{
parseError :: [Token] -> a
parseError x = error ("Error in " ++ (show $ head x))


parseExpr :: String -> Prog
parseExpr = parse . filter isNotComment . fromStringToTokens


isNotComment (ML_COMMENTS _ _ ) = False
isNotComment (COMMENTS _ _ ) = False
isNotComment _ = True


showASTTree :: (String -> Prog) -> String -> IO ()
showASTTree parser input = do
    let ast = makeTree $ parser input
    putStrLn $ drawVerticalTree ast


showAST :: (String -> Prog) -> String -> IO ()
showAST parser input = do
    let ast = makeTree $ parser input
    putStrLn $ drawTree ast

}

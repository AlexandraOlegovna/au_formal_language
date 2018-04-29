module Expr where

import Lexer

type Id = String

data Expr = TrueConst
          | FalseConst
          | Var Id
          | Num Float
          | Unary Op Expr
          | BinOp Op Expr Expr
          | FCall Id [Expr]
          deriving Eq

instance Show Expr where
    show (TrueConst) = "true"
    show (FalseConst) = "false"
    show (Var x) = "Var " ++ show x
    show (Num x) = "Num " ++ show x
    show (Unary o x) = "( " ++ show o ++ " " ++ show x ++ " )"
    show (BinOp o x y) = "( " ++ show o ++ " " ++ show x ++ " " ++ show y ++ " )"
    show (FCall x list) = "Call " ++ show x ++ " " ++ show list


data Decl = Decl Id [Id] (Maybe Stmt)
instance Show Decl where
    show (Decl name params x) | Just s <- x =
        "Func:\n  " ++ show name ++ " " ++ show params ++ "\nBody:\n" ++ show' "  " s ++ "\n"
    show (Decl name params x) | Nothing <- x =
        "Func:\n  " ++ show name ++ " " ++ show params ++ "\nBody:\n" ++ "\n"


data Prog = Prog [Decl] [Stmt]
instance Show Prog where
    show (Prog [] []) = "AST \n\n" ++ "Functions Declarations:\n\n" ++ " empty \n\n" ++ "Statments:\n\n" ++ " empty \n"
    show (Prog list []) = "AST \n\n" ++ "Functions Declarations:\n\n" ++ showF list ++ "Statments:\n\n" ++ " empty \n"
    show (Prog [] s) = "AST \n\n" ++ "Functions Declarations:\n\n" ++ " empty \n\n" ++ "Statments:\n\n" ++ showS s
    show (Prog list s) = "AST \n\n" ++ "Functions Declarations:\n\n" ++ showF list ++ "Statments:\n\n" ++ showS s

showF :: [Decl] -> String
showF [] = ""
showF (x:xs) = show x ++ showF xs

showS :: [Stmt] -> String
showS [] = ""
showS (x:xs) = show x ++ showS xs


data Stmts = St [Stmt] deriving Show

data Stmt = Assign Expr Expr
          | Write Expr
          | Read Expr
          | Return Expr
          | WhileLoop Expr [Stmt]
          | FuncCall Id [Expr]
          | IfCond Expr [Stmt] [Stmt] deriving Eq

instance Show Stmt where
    show = show' ""

show' :: String -> Stmt -> String
show' tab (Assign e1 e2) = tab ++ "Assign " ++ show e1 ++ " " ++ show e2 ++ "\n"
show' tab (Write e) = tab ++ "Write " ++ show e ++ "\n"
show' tab (Read e) = tab ++ "Read " ++ show e ++ "\n"
show' tab (WhileLoop e s) = tab ++ "While " ++ tab ++ show e ++ "\n" ++ concatMap (show' ("  "++tab)) s
show' tab (IfCond e s1 s2) = tab ++ "If " ++ "\n" ++ ("  " ++ tab) ++ show e ++ "\n" ++ tab ++ "Then" ++ "\n" ++ concatMap (show' ("  "++tab)) s1 ++ tab ++ "Else" ++ "\n" ++ concatMap (show' ("  "++tab)) s2
show' tab (FuncCall s list) = tab ++ show s ++ " " ++ show list ++ "\n"
show' tab (Return e) = tab ++ "Return " ++ show e ++ "\n"

module Expr where

import Lexer
import Data.Tree

type Id = String

class ToTree a where
    makeTree :: a -> Tree String

data Expr = TrueConst
          | FalseConst
          | Var Id
          | Num Float
          | Unary Op Expr
          | BinOp Op Expr Expr
          | FCall Id [Expr]
          deriving Eq

instance ToTree Expr where
    makeTree (TrueConst) = Node ("true") []
    makeTree (FalseConst) = Node ("false") []
    makeTree (Var x) = Node ("Var " ++ show x) []
    makeTree (Num x) = Node ("Num " ++ show x) []
    makeTree (Unary o x) = Node (show o) [makeTree x]
    makeTree (BinOp o x y) = Node (show o) [makeTree x, makeTree y]
    makeTree (FCall x list) = Node ("Call " ++ show x) (makeTree <$> list)


instance Show Expr where
    show (TrueConst) = "true"
    show (FalseConst) = "false"
    show (Var x) = "Var " ++ show x
    show (Num x) = "Num " ++ show x
    show (Unary o x) = "( " ++ show o ++ " " ++ show x ++ " )"
    show (BinOp o x y) = "( " ++ show o ++ " " ++ show x ++ " " ++ show y ++ " )"
    show (FCall x list) = "Call " ++ show x ++ " " ++ show list


data Decl = Decl Id [Expr] [Stmt]
instance Show Decl where
    show (Decl name params s)=
        "Func:\n  " ++ show name ++ " " ++ show params ++ "\nBody:\n" ++ concatMap (show' "  ") s ++ "\n"

instance ToTree Decl where
    makeTree (Decl name params s) =
        Node ("Func " ++ show name) [Node ("Params") (makeTree <$> params), Node ("Body") (makeTree <$> s)]



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


instance ToTree Prog where
    makeTree (Prog f s) =
        Node "Program" [Node ("Functions") (makeTree <$> f), Node ("Main") (makeTree <$> s)]


data Stmts = St [Stmt] deriving Show

data Stmt = Assign Expr Expr
          | Write Expr
          | Read Expr
          | Return Expr
          | WhileLoop Expr [Stmt]
          | FuncCall Id [Expr]
          | IfCond Expr [Stmt] [Stmt] deriving Eq


instance ToTree Stmt where
    makeTree (Assign e1 e2) = Node "Assign" [ Node (show e1) [], makeTree e2 ]
    makeTree (Write e) = Node "Write" [makeTree e]
    makeTree (Read e) = Node "Read" [makeTree e]
    makeTree (Return e) = Node "Return" [makeTree e]
    makeTree (WhileLoop e1 e2) = Node "While" [ makeTree e1, Node "Do" (makeTree <$> e2) ]
    makeTree (FuncCall e1 e2) = Node ("Call " ++ show e1) (makeTree <$> e2)
    makeTree (IfCond e e1 e2) = Node "If" [ makeTree e,
            Node "Then" (makeTree <$> e1),
            Node "Else" (makeTree <$> e2) ]



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

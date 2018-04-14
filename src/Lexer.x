{
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Lexer (Token(..), AlexPosn(..), alexScanTokens, fromStringToTokens) where

import Control.Monad
import Text.Parsec
import Control.Applicative hiding ((<|>))

}
%wrapper "posn"
$digit = 0-9
$alpha = [a-z]

tokens :-

	$white+									;
	\(										{ tok (\p s -> OPEN_PAR p) }
	\)										{ tok (\p s -> CLOSE_PAR p) }
	\{										{ tok (\p s -> OPEN_BRACE p) }
	\}										{ tok (\p s -> CLOSE_BRACE p) }
	\;										{ tok (\p s -> SEMI p) }
	\,										{ tok (\p s -> COMMA p) }
	if										{ tok (\p s -> IF p) }
	then									{ tok (\p s -> THEN p) }
	else									{ tok (\p s -> ELSE p) }
	while									{ tok (\p s -> WHILE p) }
	do										{ tok (\p s -> DO p) }
	read									{ tok (\p s -> READ p) }
	write									{ tok (\p s -> WRITE p) }
	function								{ tok (\p s -> FUNCTION p) }
	return									{ tok (\p s -> RETURN p) }
	true									{ tok (\p s -> TRUE p) }
	false 									{ tok (\p s -> FALSE p) }
	\:\=									{ tok (\p s -> ASSIGN p) }
	"//".*                          		{ tok (\p s -> COMMENTS p s) }
	"/*" [.\n]* "*/"                		{ tok (\p s -> ML_COMMENTS p s) }
	(\-|\+)? ($digit+ | \. | $digit+ \.) ($digit+ | e (\+|\-)? $digit+ | $digit+ e (\+|\-)? $digit+)?
											{ tok (\p s -> let Right x = parse float "" (simplify s) in NUM p x (length s)) }
	\*\*									{ tok (\p s -> OP p (convert s) (length s)) }
	(\+ | \- | \* | \/ | \% | \=\= | \!\= | \> | \>\= | \< | \<\= | \&\& | \|\|)
											{ tok (\p s -> OP p (convert s) (length s)) }
	($alpha | \_) [$alpha $digit \_ ]*		{ tok (\p s -> IDENT p s) }

{
tok f p s = f p s

data Op = Plus | Minus | Mult | Div | Mod | Pow | Eq | Ne | Gt | Ge | Lt | Le | And | Or
	deriving (Eq, Show)

convert "+" = Plus
convert "-" = Minus
convert "*" = Mult
convert "/" = Div
convert "%" = Mod
convert "**" = Pow
convert "==" = Eq
convert "!=" = Ne
convert ">" = Gt
convert ">=" = Ge
convert "<" = Lt
convert "<=" = Le
convert "&&" = And
convert "||" = Or

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b
number = many1 digit
plus = char '+' *> number
minus = char '-' <:> number
integer = plus <|> minus <|> number

float = fmap rd $ integer <++> decimal <++> exponent
    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer

simplify ('-' : num) = "-" ++ "0" ++ (filter (\s -> not $ s == '_') num)
simplify ('+' : num) = "+" ++ "0" ++ (filter (\s -> not $ s == '_') num)
simplify 		num  = 		  "0" ++ (filter (\s -> not $ s == '_') num)

-- The token type:
data Token =
	IF AlexPosn			|
	THEN AlexPosn		|
	ELSE AlexPosn		|
	WHILE AlexPosn		|
	DO AlexPosn			|
	READ AlexPosn		|
	WRITE AlexPosn		|
	FUNCTION AlexPosn	|
	RETURN AlexPosn		|
	OPEN_PAR AlexPosn	|
	CLOSE_PAR AlexPosn	|
	OPEN_BRACE AlexPosn	|
	CLOSE_BRACE AlexPosn|
	SEMI AlexPosn		|
	COMMA AlexPosn		|
	TRUE AlexPosn		|
	FALSE AlexPosn		|
	ASSIGN AlexPosn		|
	OP AlexPosn Op Int	|
	IDENT AlexPosn String	|
	COMMENTS AlexPosn String |
	ML_COMMENTS AlexPosn String |
	NUM AlexPosn Float Int
	deriving (Eq)

instance Show Token where
	show (IF (AlexPn x z y)) = "KW_If(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "if" - 1) ++ ")"
	show (THEN (AlexPn x z y)) = "KW_Then(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "then" - 1) ++ ")"
	show (ELSE (AlexPn x z y)) = "KW_Else(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "else" - 1) ++ ")"
	show (WHILE (AlexPn x z y)) = "KW_While(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "while" - 1) ++ ")"
	show (DO (AlexPn x z y)) = "KW_Do(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "do" - 1) ++ ")"
	show (READ (AlexPn x z y)) = "KW_Read(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "read" - 1) ++ ")"
	show (WRITE (AlexPn x z y)) = "KW_Write(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "write" - 1) ++ ")"
	show (FUNCTION (AlexPn x z y)) = "KW_Function(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "function" - 1) ++ ")"
	show (RETURN (AlexPn x z y)) = "KW_Return(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "return" - 1) ++ ")"
	show (OPEN_PAR (AlexPn x z y)) = "KW_Open_Par(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "(" - 1) ++ ")"
	show (CLOSE_PAR (AlexPn x z y)) = "KW_Close_Par(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length ")" - 1) ++ ")"
	show (OPEN_BRACE (AlexPn x z y)) = "KW_Open_Brace(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "{" - 1) ++ ")"
	show (CLOSE_BRACE (AlexPn x z y)) = "KW_Close_Brace(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "}" - 1) ++ ")"
	show (SEMI (AlexPn x z y)) = "Colon(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length ";" - 1) ++ ")"
	show (COMMA (AlexPn x z y)) = "Comma(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "," - 1) ++ ")"
	show (TRUE (AlexPn x z y)) = "True(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "true" - 1) ++ ")"
	show (FALSE (AlexPn x z y)) = "False(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length "false" - 1) ++ ")"
	show (ASSIGN (AlexPn x z y)) = "Assign(" ++ show z ++ ", " ++ show y ++ ", " ++ show (y + length ":=" - 1) ++ ")"
	show (OP (AlexPn x z y) o l) = "Op(" ++ show o ++ ", " ++ show z ++ ", " ++ show y ++ ", " ++ show (y + l - 1) ++ ")"
	show (COMMENTS (AlexPn x z y) s) = "Comments(" ++ show s ++ ", " ++ show z ++ ", " ++ show y ++ ", " ++ show (y + (length s) - 1) ++ ")"
	show (ML_COMMENTS (AlexPn x z y) s) = "ML_Comments(" ++ show s ++ ", " ++ show z ++ ", " ++ show y ++ ", " ++ show (y + (length s) - 1) ++ ")"
	show (IDENT (AlexPn x z y) s) = "Ident(" ++ show s ++ ", " ++ show z ++ ", " ++ show y ++", "++ show (y + (length s) - 1) ++ ")"
	show (NUM (AlexPn x z y) f l) = "Num(" ++ show f ++ ", " ++ show z ++ ", " ++ show y ++ ", " ++ show (y + l - 1) ++ ")"


fromStringToTokens :: String -> [Token]
fromStringToTokens = alexScanTokens
}

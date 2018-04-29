import Lexer
import Parser
import Expr

main :: IO ()
main = print $ runTests

runTests = (==) [] $ filter (\x -> x == False) $ concat [testWords, testOp, testIdent, testNum, testDelimetr, testExamples]

runFileTest = cmpFiles "test/test1.txt" "test/result1.txt" >>= print

testWords = [test1, test2, test3, test4, test5, test6, test7]
test1 = show (fromStringToTokens "read") == ("[KW_Read(1, 1, 4)]")
test2 = show (fromStringToTokens "if") == ("[KW_If(1, 1, 2)]")
test3 = show (fromStringToTokens "write") == ("[KW_Write(1, 1, 5)]")
test4 = show (fromStringToTokens "do") == ("[KW_Do(1, 1, 2)]")
test5 = show (fromStringToTokens "while") == ("[KW_While(1, 1, 5)]")
test6 = show (fromStringToTokens "then") == ("[KW_Then(1, 1, 4)]")
test7 = show (fromStringToTokens "else") == ("[KW_Else(1, 1, 4)]")


testOp = [test8, test9, test10, test11, test12, test13, test14, test15, test16]
test8 = show (fromStringToTokens "+") == ("[Op(Plus, 1, 1, 1)]")
test9 = show (fromStringToTokens "-") == ("[Op(Minus, 1, 1, 1)]")
test10 = show (fromStringToTokens "*") == ("[Op(Mult, 1, 1, 1)]")
test11 = show (fromStringToTokens "<=") == ("[Op(Le, 1, 1, 2)]")
test12 = show (fromStringToTokens "==") == ("[Op(Eq, 1, 1, 2)]")
test13 = show (fromStringToTokens "!=") == ("[Op(Ne, 1, 1, 2)]")
test14 = show (fromStringToTokens "%") == ("[Op(Mod, 1, 1, 1)]")
test15 = show (fromStringToTokens "&&") == ("[Op(And, 1, 1, 2)]")
test16 = show (fromStringToTokens "||") == ("[Op(Or, 1, 1, 2)]")


testIdent = [test17, test18, test19, test20, test21, test22]
test17 = show (fromStringToTokens "a") == ("[Ident(\"a\", 1, 1, 1)]")
test18 = show (fromStringToTokens "abc") == ("[Ident(\"abc\", 1, 1, 3)]")
test19 = show (fromStringToTokens "a1f1") == ("[Ident(\"a1f1\", 1, 1, 4)]")
test20 = show (fromStringToTokens "_a") == ("[Ident(\"_a\", 1, 1, 2)]")
test21 = show (fromStringToTokens "d_a") == ("[Ident(\"d_a\", 1, 1, 3)]")
test22 = show (fromStringToTokens "_c_") == ("[Ident(\"_c_\", 1, 1, 3)]")


testNum = [test25, test26, test27, test28, test29, test30, test31, test32]
test25 = show (fromStringToTokens "1") == ("[Num(1.0, 1, 1, 1)]")
test26 = show (fromStringToTokens "0.3") == ("[Num(0.3, 1, 1, 3)]")
test27 = show (fromStringToTokens ".9") == ("[Num(0.9, 1, 1, 2)]")
test28 = show (fromStringToTokens "12000000") == ("[Num(1.2e7, 1, 1, 8)]")
test29 = show (fromStringToTokens "1e2") == ("[Num(100.0, 1, 1, 3)]")
test30 = show (fromStringToTokens "1e-1") == ("[Num(0.1, 1, 1, 4)]")
test31 = show (fromStringToTokens "0e5") == ("[Num(0.0, 1, 1, 3)]")
test32 = show (fromStringToTokens "7.12") == ("[Num(7.12, 1, 1, 4)]")


testDelimetr = [test33, test34, test35]
test33 = show (fromStringToTokens "(") == ("[KW_Open_Par(1, 1, 1)]")
test34 = show (fromStringToTokens ")") == ("[KW_Close_Par(1, 1, 1)]")
test35 = show (fromStringToTokens ";") == ("[Colon(1, 1, 1)]")


testExamples = [test36, test37, test38, test39]
test36 = show (fromStringToTokens "read x; if y+1== x then write y else write x;\n//comments") == ("[KW_Read(1, 1, 4),Ident(\"x\", 1, 6, 6),Colon(1, 7, 7),KW_If(1, 9, 10),Ident(\"y\", 1, 12, 12),Op(Plus, 1, 13, 13),Num(1.0, 1, 14, 14),Op(Eq, 1, 15, 16),Ident(\"x\", 1, 18, 18),KW_Then(1, 20, 23),KW_Write(1, 25, 29),Ident(\"y\", 1, 31, 31),KW_Else(1, 33, 36),KW_Write(1, 38, 42),Ident(\"x\", 1, 44, 44),Colon(1, 45, 45),Comments(\"//comments\", 2, 1, 10)]")

test37 = show (fromStringToTokens "if 3 \n 7 \n else 8 + 9") == ("[KW_If(1, 1, 2),Num(3.0, 1, 4, 4),Num(7.0, 2, 2, 2),KW_Else(3, 2, 5),Num(8.0, 3, 7, 7),Op(Plus, 3, 9, 9),Num(9.0, 3, 11, 11)]")

test38 = show (fromStringToTokens "1e10 + 5 - _a \nm") == ("[Num(1.0e10, 1, 1, 4),Op(Plus, 1, 6, 6),Num(5.0, 1, 8, 8),Op(Minus, 1, 10, 10),Ident(\"_a\", 1, 12, 13),Ident(\"m\", 2, 1, 1)]")

test39 = show (fromStringToTokens "write f is x else ; while 7 != do 5 >= 8; \n // 5%6") == ("[KW_Write(1, 1, 5),Ident(\"f\", 1, 7, 7),Ident(\"is\", 1, 9, 10),Ident(\"x\", 1, 12, 12),KW_Else(1, 14, 17),Colon(1, 19, 19),KW_While(1, 21, 25),Num(7.0, 1, 27, 27),Op(Ne, 1, 29, 30),KW_Do(1, 32, 33),Num(5.0, 1, 35, 35),Op(Ge, 1, 37, 38),Num(8.0, 1, 40, 40),Colon(1, 41, 41),Comments(\"// 5%6\", 2, 2, 7)]")


cmpFiles a b = do
    aContents <- readFile a
    let res = show $ parseExpr aContents
    bContents <- readFile b
    return (res == bContents)

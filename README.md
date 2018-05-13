## LLang

Запустить LLang: ```stack build && stack exec -- LLang ~/example.txt```

Запустить LLang: ```stack build && stack exec -- LLang ~/example.txt large``` (для вывода большого дерева)

Запустить тесты: ```stack build && stack exec -- LLang-test```


## Конкретный синтаксис

* все выражения отделяются друг от друга `;`
* функции начинаются с ключевого слова `function`; значение возвращается с помощью `return`
* все блоки должны быть заклычены в `{``}`
* условия в `if` и `while` отделяются скобками
* сначала должно идти определение всех функций, а потом только текст основной программы

#### Синтаксический сахар:

1. Инкремент

Запись: `++var;`
``` c++
/* то же самое, что и
    x := x + 1;
*/

++x;
```

2. Сокращенные записи для сложения, разности, умножения и деления

Запись: `var (+|-|*|/)= expr;`
``` c++
/* то же самое, что и
    x := x + 1;
*/
x += 1;


/* то же самое, что и
    x := x - 1;
*/
x -= 1;


/* то же самое, что и
    x := x * 1;
*/
x *= 1;


/* то же самое, что и
    x := x / 1;
*/
x /= 1;
```

3. Условное присваивание

Запись: `var := (expr) ? expr : expr;`
``` c++
/* то же самое, что и
    x := x + 1;
*/

x := (x < 42) ? 42 : x;
```


#### Пример:
``` c++
/* function min (x, y) { return x < y;}
 function max () {}
 function min (x, y) {
     if x then {return f;} else {return x;} }
 }
 x := min(5,7);
 write(x);
*/

function test1() {
    while f + 8 && 6-9 do { //loop
        if (x ** y) then {
            if (7 <= 8+9) >= (10 < x) then {
                read(9);
                write(min(8, 5, 6, true));
            } else {
                x:=min(7,9-0);
                return false;
            }
        } else {
            while 7 do {
                read(-y);
                return true;
                return 9-0+x;
            }
        }
    }
}

function test2() {}


m := min();
x := test1();
test2();
while x do {}
while (l + 1 <= 8 && 6 % 2) do {
    write(true);
    x := x + 1;
    m := x + 1;
}

```

## Спецификация синтаксиса:

```
PROGRAM :
    | DECL* STMT*


DECL :
    | function VAR ( ARGS ) { STMT* }


STMT :
    | VAR := EXRP ;
    | while EXPR do { STMT* }
    | if EXPR then { STMT* } else { STMT*  }
    | VAR ( ARGS ) ;
    | return EXPR ;
    | write ( EXPR ) ;
    | read ( EXPR ) ;
    | ++ VAR ;
    | VAR += EXPR ;
    | VAR -= EXPR ;
    | VAR *= EXPR ;
    | VAR /= EXPR ;
    | var := ( EXPR ) ? EXPR : EXPR ;


EXPR :
    | true
    | false
    | var ( ARGS )
    | num
    | var
    | EXPR + EXPR
    | EXPR - EXPR
    | - EXPR
    | EXPR / EXPR
    | EXPR % EXPR
    | EXPR ** EXPR
    | EXPR * EXPR
    | EXPR == EXPR
    | EXPR != EXPR
    | EXPR >= EXPR
    | EXPR > EXPR
    | EXPR <= EXPR
    | EXPR < EXPR
    | EXPR && EXPR
    | EXPR || EXPR
    | ( EXPR )
```

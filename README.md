## LLang

Запустить LLang: ```stack build && stack exec -- LLang ~/example.txt```

Запустить тесты: ```stack build && stack exec -- LLang-test```


## Конкретный синтаксис

* все выражения отделяются друг от друга `;`
* функции начинаются с ключевого слова `function`; значение возвращается с помощью `return`
* все блоки должны быть заклычены в `{``}`
* условия в `if` и `while` отделяются скобками
* сначала должно идти определение всех функций, а потом только текст основной программы

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

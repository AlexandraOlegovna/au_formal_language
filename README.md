## LLang

Запустить LLang: ```stack build && stack exec -- LLang ~/example.txt```

Запустить тесты: ```stack build && stack exec -- LLang-test```


## Конкретный синтаксис

* все выражения отделяются друг от друга `;`
* функции начинаются с ключевого слова `function`; значение возвращается с помощью `return`
* все блоки должны быть заклычены в `{``}`
* условия в `if` и `while` отделяются скобками

#### Пример:
```
function min (x, y) {
  if (x < y) {
    return x;
  }
  else {
    return y;
  }
}

x := min(5,7;)
write(x);
```

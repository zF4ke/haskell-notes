# Table of Contents

- [Teste de funções com QuickCheck](#teste-de-funções-com-quickcheck)
    - [Definição de propriedades](#definição-de-propriedades)
    - [Teste de encontro a um modelo](#teste-de-encontro-a-um-modelo)
    - [Funções ajudantes](#funções-ajudantes)
    - [Propriedades condicionais](#propriedades-condicionais)

# Teste de funções com QuickCheck

A ferramenta QuickCheck permite definir propriedades das funções a testar. As propriedades são depois validadas num grande número de casos. O QuickCheck também é capaz de gerar casos de teste aleatórios. Utiliza um método simples, geração aleatória, que funciona muito bem na prática.

## Definição de propriedades

Consideremos a função que inverte uma lista recorrendo a um parâmetro de acumulação. Como a função `reverse` faz parte do `Prelude`, chamamos à nossa função `reverse'`.

```hs
reverse' :: [a] -> [a]
reverse' = rev []
    where rev acc (x:xs) = rev (x:acc) xs
          rev acc [] = acc
```

Pretendemos agora testar algumas propriedades da função `reverse'`. O que constitui uma propriedade interessante? Procuramos uma expressão que relacione a função `reverse'` com outras funções (ou consigo própria) e que seja válida para todas as listas finitas. Uma propriedade que relaciona `reverse'` consigo própria é a seguinte:

*A inversa da inversa é a lista original.*

Uma propriedade que relaciona `reverse'` com a função `length` toma a seguinte forma:

*Uma lista e a sua inversa têm sempre o mesmo comprimento.*

Com o QuickCheck verificamos propriedades para valores finitos, neste caso para listas finitas. Para verificar se a nossa função de inversão verifica esta propriedade, começamos por escrever o predicado `prop_reverse_length`. Notem a convenção do QuickCheck: os nomes das propriedades começam sempre por `prop_`.

A propriedade é verificada se a função correspondente devolver `True` para qualquer argumento. No entanto, para testar a função precisamos de decidir qual o tipo dos elementos nas listas de teste; lembrem-se que a função `reverse'` é polimórfica. Como escolhemos o tipo dos elementos das listas de teste? Acontece que a função a testar não depende do tipo de dados: basta olhar para assinatura: `reverse' :: [a] -> [a]`. Deste modo o tipo escolhido não afeta o resultado, desde que haja valores "suficientes" no tipo. Se escolhermos o tipo `Int`, podemos restringir o tipo da função de teste. Eis uma possível definição:

```hs
prop_reverse_length :: [Int] -> Bool
prop_reverse_length xs = length (reverse' xs) == length xs
```

Agora podemos correr uns testes.

```hs
ghci> prop_reverse_length []
True
ghci> prop_reverse_length [8]
True
ghci> prop_reverse_length [4,0,1,-2,9,-3]
True
```

Funciona! (para estes três casos, bem entendido). Mas este método de teste é enfadonho. Melhor seria se a máquina fizésse este trabalho por nós. Para automatizar o processo de testes usamos o módulo QuickTest.

```hs
import Test.QuickCheck
```

O QuickCheck gera dados de entrada aleatórios, tais como as três listas acima e passa-os para a propriedade escolhida usando a função quickCheck. A função recebe uma propriedade como argumento e aplica-a a um grande número de argumentos gerados aleatoriamente — 100 por omissão — reportando OK se o resultado for `True` em todos os casos.

```hs
ghci> :m Test.QuickCheck
ghci> quickCheck prop_reverse_length
+++ OK, passed 100 tests.
```

Se a propriedade falhar, a função quickCheck apresenta um contra exemplo. Por exemplo, se definirmos erradamente:

```hs
reverse' :: [a] -> [a]
reverse' xs = rev xs []
    where rev (x:xs) acc = rev xs (x:acc)
          rev [] acc = [] -- erro
```

então o QuickCheck pode produzir o seguinte resultado:

```hs
ghci> quickCheck prop_reverse_length
*** Failed! Falsifiable (after 5 tests and 4 shrinks):
[0]
```

O contra exemplo pode ser recuperado tomando a lista [0] como o argumento xs da propriedade `prop_reverse_length`. Em particular temos

```hs
ghci> length (reverse' [0]) == 0
ghci> length [0] == 1
```

## Teste de encontro a um modelo

Consideremos agora a função de ordenação por inserção, definida deste modo:

```hs
sort :: Ord a => [a] -> [a]
sort = foldr insert []

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x <= y = x : y : ys
    | otherwise = y : insert x ys
```

Comecemos pela função `sort`. A maneira mais económica de testar a função é recorrendo a outra função na qual confiamos, por exemplo, a função sort do módulo `Data.List`. Neste caso, só precisamos de dizer que as duas funções devem produzir o mesmo resultado.

```hs
import qualified Data.List as List

prop_sort_is_sort :: [Int] -> Bool
prop_sort_is_sort xs = sort xs == List.sort xs
```

Para correr os nossos testes chamamos o QuickCheck.

```hs
ghci> quickCheck prop_sort_is_sort
+++ OK, passed 100 tests.
```

## Funções ajudantes

Mas, e se não confiarmos na implementação List.sort? Neste caso temos de pensar em propriedades que caracterizem as funções de ordenação. Dizemos que uma dada função é de ordenação se

*O resultado está ordenado, e*

*O resultado tem exactamente os mesmos elementos que a lista original.*

Para especificar a primeira propriedade definimos uma função ajudante
sorted que verifica se uma lista está ordenada.

```hs
sorted :: Ord a => [a] -> Bool
sorted xs = and $ zipWith (<=) xs (tail xs)
```

Agora é fácil de escrever a propriedade que garante a ordenação do resultado.

```hs
prop_sort_sorted :: [Int] -> Bool
prop_sort_sorted xs = sorted (sort xs)
```

Para a segunda propriedade precisamos também de definir uma função ajudante que decida se duas listas têm os mesmos elementos.

```hs
import qualified Data.List as List

sameElems :: Eq a => [a] -> [a] -> Bool
xs `sameElems` ys = xs List.\\ ys == ys List.\\ xs
```

Equipados com a função sameElems podemos facilmente escrever a segunda propriedade.

```hs
prop_sort_same_elems :: [Int] -> Bool
prop_sort_same_elems xs = sort xs `sameElems` xs
```

Para testar a função de ordenação de encontro a propriedades, corremos os dois testes.

```hs
ghci> quickCheck prop_sort_sorted
+++ OK, passed 100 tests.
ghci> quickCheck prop_sort_same_elems
+++ OK, passed 100 tests.
```

## Propriedades condicionais

É boa prática definir e testar propriedades de muitas (ou todas) as funções envolvidas num programa, ao invés de testar apenas a função principal. O teste de cada função individual pode conduzir à descoberta de mais erros, para além de que fica mais fácil encontrar a fonte de um erro. Pensemos então na função de inserção e imaginemos que não temos outra função à qual recorrer. As duas propriedades de interesse são as seguintes:

*Se o argumento estiver ordenado, então também o resultado deverá estar, e*

*Os elementos no resultado devem ser aqueles constantes nos dois argumentos da função.*

A segunda propriedade é fácil de verificar. Escrevemos a seguinte função:

```hs
prop_insert_same_elems :: Int -> [Int] -> Bool
prop_insert_same_elems x xs =
insert x xs `sameElems` (x:xs)
```

A primeira é mais problemática. Não se trata de uma simples propriedade equacional, é uma propriedade condicional: só podemos esperar uma lista ordenada à saída se apresentarmos uma lista ordenada à entrada. O QuickCheck dispõe de um combinador de implicação ==> para representar propriedades condicionais. Usando implicação a segunda propriedade fica assim:

```hs
prop_insert_sorted :: Int -> [Int] -> Property
prop_insert_sorted x xs =
sorted xs ==> sorted (insert x xs)
```

A verificação deste tipo de propriedades funciona de um modo diferente daquele que vimos em ação até ao momento. Em vez de verificar a propriedade para 100 casos de teste aleatórios, o QuickCheck tenta 100 casos de teste que satisfaçam a pré-condição. Candidato que não esteja de acordo com a pré-condição (que não esteja ordenado) é mandado fora e um novo candidato é utilizado. Deste modo quando o QuickCheck diz que o predicado é válido para 100 testes ficamos com a certeza que todos eles passaram a pré-condição. Como o funcionamento destes testes é diferente mudamos o tipo da propriedade de Bool para Property. O processo de teste é semelhante, mas o resultado pode ser diferente:

```hs
ghci> quickCheck prop_insert_sorted
*** Gave up! Passed only 76 tests; 1000 discarded tests.
```

Quando a pré-condição é raramente satisfeita, podemos acabar por gerar muitos casos sem encontrar nenhum que verifique a pré-condição. Em vez de executar indefinidamente, a função quickCheck gera apenas um número fixo de testes (1000 por omissão). Se a função quickCheck não encontrar 100 testes que passem a pré-condição nas primeiras 1000 tentativas, desiste. Ficamos no entanto a saber quantos testes verificaram a pré-condição (76 neste caso). 
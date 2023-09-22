# Haskell

- [Basics](#basics)
- [Starting Out](#starting-out)
  - [Arithmetic](#arithmetic)
  - [Boolean Algebra](#boolean-algebra)
  - [Functions](#functions)
- [Baby's first functions](#babys-first-functions)
  - [Expressions](#expressions)
  - [Definitions](#definitions)
- [An intro to lists](#an-intro-to-lists)

# Basics

Haskell is a purely functional programming language.

- Pure (mathematical functions): Functions in the mathematical definition, where they have an input and an output.

- Immutable data.

- No/Less side-effects: It's very hard to verify the absence of side-effects and they can be dangerous.

- Declarative.

- Easier to verify: Mathematically prove the correctness of an algorithm.

- Lazy.

- Statically typed.

# Starting out

## Arithmetic

```hs
╰─❯ ghci
ghci> 2 + 15
17

ghci> 34 * 3
102

ghci> 5/6
0.8333333333333334

ghci> 7/0
Infinity

ghci> 1/0
Infinity

ghci> 0/0
NaN

ghci> 0**1
0.0

ghci> 0**0
1.0

ghci> 5 * -3
<interactive>:9:1: error:
    Precedence parsing error
        cannot mix `*' [infixl 7] and prefix `-' [infixl 6] in the same infix expression

ghci> 5 * (-3)
-15
```

## Boolean Algebra

```hs
ghci> not False
True

ghci> False || True
True

ghci> 5 == 5
True

ghci> 5 /= 5
False

ghci> "5" == "5"
True

ghci> "5" == 5
<interactive>:20:8: error:
    * No instance for (Num String) arising from the literal `5'
    * In the second argument of `(==)', namely `5'
      In the expression: "5" == 5
      In an equation for `it': it = "5" == 5

ghci> 5 + "llama"
<interactive>:24:3: error:
    * No instance for (Num String) arising from a use of `+'
    * In the expression: 5 + "llama"
      In an equation for `it': it = 5 + "llama"

ghci> "5" + "llama"
<interactive>:23:5: error:
    * No instance for (Num String) arising from a use of `+'
    * In the expression: "5" + "llama"
      In an equation for `it': it = "5" + "llama"
```

## Functions

`'succ' stands for successor.`

```hs
ghci> succ 8
9
```

```hs
ghci> min 5 2
2
```

```hs
ghci> max 5.6 7
7.0
```

Function application (calling a function by putting a space after it and then typing out the parameters) has the highest precedence of them all. What that means for us is that these two statements are equivalent.

```hs
ghci> succ 9 + max 5 4 + 1
16
ghci> (succ 9) + (max 5 4) + 1
16
```

```hs
ghci> succ (max 5 6 + 1)
8
```

# Baby's first functions

## Expressions

In the previous section we got a basic feel for calling functions. Now let's try making our own!

Open up your favorite text editor and punch in this function that takes a number and multiplies it by two.

```hs
doubleMe x = x + x
```

Functions are defined in a similar way that they are called. The function name is followed by parameters separated by spaces. But when defining functions, there's a `=` and after that we define what the function does. Save this as `baby.hs` or something.

```hs
ghci> :l baby
[1 of 1] Compiling Main             ( baby.hs interpreted )
Ok, one module loaded.
```

```hs
ghci> doubleMe 9
18
ghci> doubleMe 0
0
ghci> doubleMe 2.3
4.6
```

Let's make a function that takes two numbers and multiplies each by two and then adds them together.

```hs
doubleUs x y = x*2 + y*2
```

```hs
ghci> doubleUs 2 3
10
```

As expected, you can call your own functions from other functions that you made. With that in mind, we could redefine doubleUs like this:

```hs
doubleUs x y = doubleMe x + doubleMe y
```

Functions in Haskell don't have to be in any particular order, so it doesn't matter if you define doubleMe first and then doubleUs or if you do it the other way around.

Now we're going to make a function that multiplies a number by 2 but only if that number is smaller than or equal to 100 because numbers bigger than 100 are big enough as it is!

```hs
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2
```

Right here we introduced Haskell's if statement. You're probably familiar with if statements from other languages. The difference between Haskell's if statement and if statements in imperative languages is that the else part is mandatory in Haskell. In imperative languages you can just skip a couple of steps if the condition isn't satisfied but in Haskell every expression and function must return something.

Another thing about the if statement in Haskell is that it is an _expression_. Because the else is mandatory, an if statement will always return something and that's why it's an expression.

If we wanted to add one to every number that's produced in our previous function, we could have written its body like this.

```hs
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
```

Had we omitted the parentheses, it would have added one only if `x` wasn't greater than 100. Note the `'` at the end of the function name. That apostrophe doesn't have any special meaning in Haskell's syntax. It's a valid character to use in a function name. We usually use `'` to either denote a strict version of a function (one that isn't lazy) or a slightly modified version of a function or a variable. Because `'` is a valid character in functions, we can make a function like this.

## Definitions

```hs
conanO'Brien = "It's a-me, Conan O'Brien!"
```

There are two noteworthy things here. The first is that in the function name we didn't capitalize Conan's name. That's because **functions can't begin with uppercase letters**. We'll see why a bit later. The second thing is that this function doesn't take any parameters. When a function doesn't take any parameters, we usually say it's a **definition** (or a _name_). Because we can't change what names (and functions) mean once we've defined them, `conanO'Brien` and the string `"It's a-me, Conan O'Brien!"` can be used interchangeably.

# An intro to lists

In this section we'll look at the basics of lists, strings (which are lists) and list comprehensions.

In Haskell, lists are a homogenous data structure. It stores several elements of the same type. That means that we can have a list of integers or a list of characters but we can't have a list that has a few integers and then a few characters. And now, a list!

> **Note:** We can use the `let` keyword to define a name right in GHCI. Doing`let a = 1` inside GHCI is the equivalent of writing `a = 1` in a script and then loading it.

```hs
ghci> let lostNumbers = [4,8,15,16,23,42]
ghci> lostNumbers
[4,8,15,16,23,42]
```

If we tried a list like `[1,2,'a',3,'b','c',4]`, Haskell would complain that characters (which are, by the way, denoted as a character between single quotes) are not numbers. Speaking of characters, strings are just lists of characters. `"hello"` is just syntactic sugar for `['h','e','l','l','o']`. Because strings are lists, we can use list functions on them, which is really handy.

A common task is putting two lists together. This is done by using the `++` operator.

```hs
ghci> [1,2,3,4] ++ [9,10,11,12]
[1,2,3,4,9,10,11,12]

ghci> "hello" ++ " " ++ "world"
"hello world"

ghci> ['w','o'] ++ ['o','t']
"woot"
```

Watch out when repeatedly using the `++` operator on long strings. When you put together two lists (even if you append a singleton list to a list, for instance: `[1,2,3] ++ [4]`), internally, Haskell has to walk through the whole list on the left side of `++`. That's not a problem when dealing with lists that aren't too big. But putting something at the end of a list that's fifty million entries long is going to take a while. However, putting something at the beginning of a list using the `:` operator (also called the cons operator) is instantaneous.

```hs
ghci> 'A':" SMALL CAT"
"A SMALL CAT"

ghci> 5:[1,2,3,4,5]
[5,1,2,3,4,5]

ghci> 1:2:[]
[1,2]
```

Notice how `:` takes a number and a list of numbers or a character and a list of characters, whereas `++` takes two lists. Even if you're adding an element to the end of a list with `++`, you have to surround it with square brackets so it becomes a list.

`[1,2,3]` is actually just syntactic sugar for `1:2:3:[]`. `[]` is an empty list. If we prepend `3` to it, it becomes `[3]`. If we prepend `2` to that, it becomes `[2,3]`, and so on.

If you want to get an element out of a list by index, use `!!`. The indices start at 0.

```hs
ghci> [1,2,3] !! 1
2

ghci> "sup" !! 0
's'

ghci> [1,2,3] !! 3
*** Exception: Prelude.!!: index too large
```

Lists can also contain lists. They can also contain lists that contain lists that contain lists …

```hs
ghci> let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
ghci> b
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]

ghci> b ++ [2]
<interactive>:43:1: error:
    • Non type-variable argument in the constraint: Num [a]
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall {a}. (Num a, Num [a]) => [[a]]

ghci> b ++ [[1]]
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1]]

ghci> [[3]] : b
<interactive>:44:1: error:
    • Non type-variable argument in the constraint: Num [a]
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall {a}. (Num a, Num [a]) => [[[a]]]

ghci> [3] : b
[[3],[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]

ghci> b
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
ghci> b !! 1
[5,3,3,3]
```

Lists can be compared if the stuff they contain can be compared. When using `<`, `<=`, `>` and `>=` to compare lists, they are compared in lexicographical order. First the heads are compared. If they are equal then the second elements are compared, etc.

```hs
ghci> [3,2,1] > [2,1,0]
True
ghci> [3,2,1] > [2,10,100]
True
ghci> [3,4,2] > [3,4]
True
ghci> [3,4,2] > [2,4]
True
ghci> [3,4,2] == [3,4,2]
True
ghci> [3,4] >= [3,4,0]
False
```

`head` takes a list and returns its head. The head of a list is basically its first element.

```hs
ghci> head [5,4,3,2,1]
5
```

`tail` takes a list and returns its tail. In other words, it chops off a list's head.

```hs
ghci> tail [5,4,3,2,1]
[4,3,2,1]
```

`last` takes a list and returns its last element.

```hs
ghci> last [5,4,3,2,1]
1
```

`init` takes a list and returns everything except its last element.

```hs
ghci> init [5,4,3,2,1]
[5,4,3,2]
```

If we think of a list as a monster, here's what's what.

<img style="background-color: white" alt="list monster" src="https://s3.amazonaws.com/lyah/listmonster.png">

But what happens if we try to get the head of an empty list?

```hs
ghci> head []
*** Exception: Prelude.head: empty list
```

Oh my! It all blows up in our face! If there's no monster, it doesn't have a head. When using head, tail, last and init, be careful not to use them on empty lists. This error cannot be caught at compile time so it's always good practice to take precautions against accidentally telling Haskell to give you some elements from an empty list.

`length` takes a list and returns its length, obviously.

```hs
ghci> length [5,4,3,2,1]
5
```

`null` checks if a list is empty. If it is, it returns True, otherwise it returns False. Use this function instead of xs == [] (if you have a list called xs)

```hs
ghci> null [1,2,3]
False
ghci> null []
True
```

`reverse` reverses a list.

```hs
ghci> reverse [5,4,3,2,1]
[1,2,3,4,5]
```

`take` takes number and a list. It extracts that many elements from the beginning of the list. Watch.

```hs
ghci> take 3 [5,4,3,2,1]
[5,4,3]
ghci> take 1 [3,9,3]
[3]
ghci> take 5 [1,2]
[1,2]
ghci> take 0 [6,6,6]
[]
```

See how if we try to take more elements than there are in the list, it just returns the list. If we try to take 0 elements, we get an empty list.

`drop` works in a similar way, only it drops the number of elements from the beginning of a list.

```hs
ghci> drop 3 [8,4,2,1,5,6]
[1,5,6]
ghci> drop 0 [1,2,3,4]
[1,2,3,4]
ghci> drop 100 [1,2,3,4]
[]
```

`maximum` takes a list of stuff that can be put in some kind of order and returns the biggest element.

`minimum` returns the smallest.s

```hs
ghci> minimum [8,4,2,1,5,6]
1
ghci> maximum [1,9,2,3,4]
9
```

`sum` takes a list of numbers and returns their sum.

`product` takes a list of numbers and returns their product.

```hs

```

```hs

```

```hs

```

```hs

```

```hs

```

```hs

```

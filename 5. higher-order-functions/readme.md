# Table of Contents

- [Curried functions](#curried-functions)
- [Some higher-orderism is in order](#some-higher-orderism-is-in-order)
- [Maps and filters](#maps-and-filters)
- [Lambdas](#lambdas)
- [Only folds and horses](#only-folds-and-horses)
- [Function application with $](#function-application-with)
- [Function composition](#function-composition)

Haskell functions can take functions as parameters and return functions as return values. A function that does either of those is called a higher order function. Higher order functions aren't just a part of the Haskell experience, they pretty much are the Haskell experience. It turns out that if you want to define computations by defining what stuff is instead of defining steps that change some state and maybe looping them, higher order functions are indispensable. They're a really powerful way of solving problems and thinking about programs.

# Curried functions

Every function in Haskell officially only takes one parameter. So how is it possible that we defined and used several functions that take more than one parameter so far? Well, it's a clever trick! All the functions that accepted several parameters so far have been **curried functions**. What does that mean? You'll understand it best on an example. Let's take our good friend, the `max` function. It looks like it takes two parameters and returns the one that's bigger. Doing `max 4 5` first creates a function that takes a parameter and returns either `4` or that parameter, depending on which is bigger. Then, `5` is applied to that function and that function produces our desired result. That sounds like a mouthful but it's actually a really cool concept. The following two calls are equivalent:

```hs
ghci> max 4 5
5
ghci> (max 4) 5
5
```

Putting a space between two things is simply function application. The space is sort of like an operator and it has the highest precedence. Let's examine the type of `max`. It's `max :: (Ord a) => a -> a -> a`. That can also be written as `max :: (Ord a) => a -> (a -> a)`. That could be read as: `max` takes an a and returns (that's the `->`) a function that takes an `a` and returns an `a`. That's why the return type and the parameters of functions are all simply separated with arrows.

So how is that beneficial to us? Simply speaking, if we call a function with too few parameters, we get back a **partially applied** function, meaning a function that takes as many parameters as we left out. Using partial application (calling functions with too few parameters, if you will) is a neat way to create functions on the fly so we can pass them to another function or to seed them with some data.

```hs
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z
```

What really happens when we do `multThree 3 5 9` or `((multThree 3) 5) 9`? First, `3` is applied to `multThree`, because they're separated by a space. That creates a function that takes one parameter and returns a function. So then `5` is applied to that, which creates a function that will take a parameter and multiply it by 15. `9` is applied to that function and the result is 135 or something.

Take a look at this:

```hs
ghci> let multTwoWithNine = multThree 9
ghci> multTwoWithNine 2 3
54
ghci> let multWithEighteen = multTwoWithNine 2
ghci> multWithEighteen 10
180
```

By calling functions with too few parameters, so to speak, we're creating new functions on the fly. What if we wanted to create a function that takes a number and compares it to `100`? We could do something like this:

```hs
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x
```

Now let's think about what `compare 100` returns. It returns a function that takes a number and compares it with `100`. Wow! Isn't that the function we wanted? We can rewrite this as:

```hs
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100
```

Infix functions can also be partially applied by using sections. To section an infix function, simply surround it with parentheses and only supply a parameter on one side. That creates a function that takes one parameter and then applies it to the side that's missing an operand. An insultingly trivial function:

```hs
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
```

# Some higher-orderism is in order

Functions can take functions as parameters and also return functions. To illustrate this, we're going to make a function that takes a function and then applies it twice to something!

```hs
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
```

First of all, notice the type declaration. Before, we didn't need parentheses because `->` is naturally right-associative. However, here, they're **mandatory**. They indicate that the first parameter is a function that takes something and returns that same thing. The second parameter is something of that type also and the return value is also of the same type.

```hs
ghci> applyTwice (+3) 10
16
ghci> applyTwice (++ " HAHA") "HEY"
"HEY HAHA HAHA"
ghci> applyTwice ("HAHA " ++) "HEY"
"HAHA HAHA HEY"
ghci> applyTwice (multThree 2 2) 9
144
ghci> applyTwice (3:) [1]
[3,3,1]
```

Now we're going to use higher order programming to implement a really useful function that's in the standard library. It's called `zipWith`. It takes a function and two lists as parameters and then joins the two lists by applying the function between corresponding elements. Here's how we'll implement it:

```hs
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
```

# Maps and filters

`map` takes a function and a list and applies that function to every element in the list, producing a new list. Let's see what its type signature is and how it's defined.

```hs
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
```

```hs
ghci> map (+3) [1,5,3,1,6]
[4,8,6,4,9]
ghci> map (++ "!") ["BIFF", "BANG", "POW"]
["BIFF!","BANG!","POW!"]
ghci> map (replicate 3) [3..6]
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
[[1,4],[9,16,25,36],[49,64]]
ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
[1,3,6,2,2]
```

`filter` is a function that takes a predicate (a predicate is a function that tells whether something is true or not, so in our case, a function that returns a boolean value) and a list and then returns the list of elements that satisfy the predicate. The type signature and implementation go like this:

```hs
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
```

Let's find **the largest number under 100,000 that's divisible by 3829**. To do that, we'll just filter a set of possibilities in which we know the solution lies.

```hs
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0
```

Next up, we're going to find **the sum of all odd squares that are smaller than 10,000**. But first, because we'll be using it in our solution, we're going to introduce the `takeWhile` function. It takes a predicate and a list and then goes from the beginning of the list and returns its elements while the predicate holds true. Once an element is found for which the predicate doesn't hold, it stops. If we wanted to get the first word of the string `"elephants know how to party"`, we could do `takeWhile (/=' ') "elephants know how to party"` and it would return `"elephants"`.

Okay. The sum of all odd squares that are smaller than 10,000. First, we'll begin by mapping the `(^2)` function to the infinite list `[1..]`. Then we filter them so we only get the odd ones. And then, we'll take elements from that list while they are smaller than 10,000. Finally, we'll get the sum of that list. We don't even have to define a function for that, we can do it in one line in GHCI:

```hs
ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
166650
```

# Lambdas

Lambdas are basically anonymous functions that are used because we need some functions only once. Normally, we make a lambda with the sole purpose of passing it to a higher-order function. To make a lambda, we write a `\` (because it kind of looks like the greek letter lambda if you squint hard enough) and then we write the parameters, separated by spaces. After that comes a `->` and then the function body. We usually surround them by parentheses, because otherwise they extend all the way to the right.

If you look about 5 inches up, you'll see that we used a where binding in our `numLongChains` function to make the `isLong` function for the sole purpose of passing it to `filter`. Well, instead of doing that, we can use a lambda:

```hs
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
```

The only difference is that you can't define several patterns for one parameter, like making a `[]` and a `(x:xs)` pattern for the same parameter and then having values fall through. If a pattern matching fails in a lambda, a runtime error occurs, so be careful when pattern matching in lambdas!

# Only folds and horses

Back when we were dealing with recursion, we noticed a theme throughout many of the recursive functions that operated on lists. Usually, we'd have an edge case for the empty list. We'd introduce the `x:xs` pattern and then we'd do some action that involves a single element and the rest of the list. It turns out this is a very common pattern, so a couple of very useful functions were introduced to encapsulate it. These functions are called folds. They're sort of like the `map` function, only they reduce the list to some single value.

A fold takes a binary function, a starting value (I like to call it the accumulator) and a list to fold up. The binary function itself takes two parameters. The binary function is called with the accumulator and the first (or last) element and produces a new accumulator. Then, the binary function is called again with the new accumulator and the now new first (or last) element, and so on. Once we've walked over the whole list, only the accumulator remains, which is what we've reduced the list to.

First let's take a look at the **`foldl`** function, also called the left fold. It folds the list up from the left side. The binary function is applied between the starting value and the head of the list. That produces a new accumulator value and the binary function is called with that value and the next element, etc.

Let's implement `sum` again, only this time, we'll use a fold instead of explicit recursion.

```hs
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
```

<img style="background-color: white" alt="list monster" src="https://learnyouahaskell.github.io/assets/images/higher-order-functions/foldl.png">

This professional diagram on the left illustrates how a fold happens, step by step (day by day!). The greenish brown number is the accumulator value. You can see how the list is sort of consumed up from the left side by the accumulator. Om nom nom nom! If we take into account that functions are curried, we can write this implementation ever more succinctly, like so:

```hs
sum' :: (Num a) => [a] -> a  
sum' = foldl (+) 0  
```

**Folds can be used to implement any function where you traverse a list once, element by element, and then return something based on that. Whenever you want to traverse a list to return something, chances are you want a fold.**

The `foldl1` and `foldr1` functions work much like `foldl` and `foldr`, only you don't need to provide them with an explicit starting value. They assume the first (or last) element of the list to be the starting value and then start the fold with the element next to it. With that in mind, the sum function can be implemented like so: `sum = foldl1 (+)`. Because they depend on the lists they fold up having at least one element, they cause runtime errors if called with empty lists. `foldl` and `foldr`, on the other hand, work fine with empty lists. When making a fold, think about how it acts on an empty list. If the function doesn't make sense when given an empty list, you can probably use a `foldl1` or `foldr1` to implement it.

Just to show you how powerful folds are, we're going to implement a bunch of standard library functions by using folds:

```hs
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
    
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
    
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
    
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
    
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
    
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  
```

**`scanl`** and **`scanr`** are like `foldl` and `foldr`, only they report all the intermediate accumulator states in the form of a list. There are also `scanl1` and `scanr1`, which are analogous to `foldl1` and `foldr1`.

```hs
ghci> scanl (+) 0 [3,5,2,1]  
[0,3,8,10,11]  
ghci> scanr (+) 0 [3,5,2,1]  
[11,8,3,1,0]  
ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  
[3,4,5,5,7,9,9,9]  
ghci> scanl (flip (:)) [] [3,2,1]  
[[],[3],[2,3],[1,2,3]]  
```

When using a `scanl`, the final result will be in the last element of the resulting list while a `scanr` will place the result in the head.

Scans are used to monitor the progression of a function that can be implemented as a fold. Let's answer us this question: **How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?** To get the squares of all natural numbers, we just do `map sqrt [1..]`. Now, to get the sum, we could do a fold, but because we're interested in how the sum progresses, we're going to do a scan. Once we've done the scan, we just see how many sums are under 1000. The first sum in the scanlist will be 1, normally. The second will be 1 plus the square root of 2. The third will be that plus the square root of 3. If there are X sums under 1000, then it takes X+1 elements for the sum to exceed 1000.

```hs
sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1  
```

# Function application with $

Alright, next up, we'll take a look at the $ function, also called *function application*. First of all, let's check out how it's defined:

```hs
($) :: (a -> b) -> a -> b  
f $ x = f x  
```

What the heck? What is this useless operator? It's just function application! Well, almost, but not quite! Whereas normal function application (putting a space between two things) has a really high precedence, the `$` function has the lowest precedence. Function application with a space is left-associative (so f a b c is the same as `(((f a) b) c)`), function application with `$` is right-associative.

That's all very well, but how does this help us? Most of the time, it's a convenience function so that we don't have to write so many parentheses. Consider the expression `sum (map sqrt [1..130])`. Because `$` has such a low precedence, we can rewrite that expression as `sum $ map sqrt [1..130]`, saving ourselves precious keystrokes!

How about `sum (filter (> 10) (map (*2) [2..10]))`? Well, because `$` is right-associative, `f (g (z x))` is equal to `f $ g $ z x`. And so, we can rewrite `sum (filter (> 10) (map (*2) [2..10]))` as `sum $ filter (> 10) $ map (*2) [2..10]`. 

But apart from getting rid of parentheses, `$` means that function application can be treated just like another function. That way, we can, for instance, map function application over a list of functions.

```hs
ghci> map ($ 3) [(4+), (10*), (^2), sqrt]  
[7.0,30.0,9.0,1.7320508075688772]  
```

# Function composition

In mathematics, function composition is defined like this: `(f . g)(x) = f(g(x))`, meaning that composing two functions produces a new function that, when called with a parameter, say, x is the equivalent of calling g with the parameter x and then calling the f with that result.

In Haskell, function composition is pretty much the same thing. We do function composition with the `.` function, which is defined like so:

```hs
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)  
```

Here are some examples:

```hs
k = a . b . c $ value
```

```hs
ghci> sum $ filter (> 10) $ map (*2) [2..10]
80
ghci> sum . filter (> 10) . map (*2) $ [2..10]
80
```

```hs
oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  
```

However, if there was a chance of someone else reading that code, I would have written it like this:

```hs
oddSquareSum :: Integer  
oddSquareSum = 
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit  
```

It wouldn't win any code golf competition, but someone reading the function will probably find it easier to read than a composition chain.
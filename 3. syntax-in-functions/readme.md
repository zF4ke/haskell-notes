# Table of Contents

- [Pattern matching](#pattern-matching)
- [Guards, guards!](#guards-guards)

# Pattern matching

When defining functions, you can define separate function bodies for different patterns. This leads to really neat code that's simple and readable. You can pattern match on any data type — numbers, characters, lists, tuples, etc. Let's make a really trivial function that checks if the number we supplied to it is a seven or not.


```hs
lucky :: (Integral a) => a -> String      
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"
```

When you call `lucky`, the patterns will be checked from top to bottom and when it conforms to a pattern, the corresponding function body will be used. The only way a number can conform to the first pattern here is if it is 7. If it's not, it falls through to the second pattern, which matches anything and binds it to `x`. This function could have also been implemented by using an if statement. But what if we wanted a function that says the numbers from 1 to 5 and says `"Not between 1 and 5"` for any other number? Without pattern matching, we'd have to make a pretty convoluted if then else tree. However, with it:

```hs
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  
```

Note that if we moved the last pattern (the catch-all one) to the top, it would always say "Not between 1 and 5", because it would catch all the numbers and they wouldn't have a chance to fall through and be checked for any other patterns.

Remember the factorial function we implemented previously? We defined the factorial of a number n as `product [1..n]`. We can also define a factorial function recursively, the way it is usually defined in mathematics.

```hs
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  
```

Pattern matching can also fail. If we define a function like this: 

```hs
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"  
```

and then try to call it with an input that we didn't expect, this is what happens:

```hs
ghci> charName 'a'  
"Albert"  
ghci> charName 'b'  
"Broseph"  
ghci> charName 'h'  
"*** Exception: tut.hs:(53,0)-(55,21): Non-exhaustive patterns in function charName  
```

Pattern matching can also be used on tuples. What if we wanted to make a function that takes two vectors in a 2D space (that are in the form of pairs) and adds them together? To add together two vectors, we add their x components separately and then their y components separately. Here's how we would have done it if we didn't know about pattern matching:

```hs
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)  
```

Well, that works, but there's a better way to do it. Let's modify the function so that it uses pattern matching.

```hs
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  
```

`fst` and `snd` extract the components of pairs. But what about triples? Well, there are no provided functions that do that but we can make our own.

```hs
first :: (a, b, c) -> a  
first (x, _, _) = x  

second :: (a, b, c) -> b  
second (_, y, _) = y  

third :: (a, b, c) -> c  
third (_, _, z) = z  
```

The `_` means the same thing as it does in list comprehensions. It means that we really don't care what that part is, so we just write a `_`.

Which reminds me, you can also pattern match in list comprehensions. Check this out:

```hs
ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
ghci> [a+b | (a,b) <- xs]  
[4,7,6,8,11,4]   
```

Lists themselves can also be used in pattern matching. You can match with the empty list `[]` or any pattern that involves `:` and the empty list. But since `[1,2,3]` is just syntactic sugar for `1:2:3:[],` you can also use the former pattern. A pattern like `x:xs` will bind the head of the list to `x` and the rest of it to `xs`, even if there's only one element so `xs` ends up being an empty list. 

Now that we know how to pattern match against list, let's make our own implementation of the `head` function.

```hs
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  
```

Let's make a trivial function that tells us some of the first elements of the list in (in)convenient English form.

```hs
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  
```

Note that `(x:[])` and `(x:y:[])` could be rewritten as `[x]` and `[x,y]` (because its syntactic sugar, we don't need the parentheses). We can't rewrite `(x:y:_)` with square brackets because it matches any list of length 2 or more.

We already implemented our own `length` function using list comprehension. Now we'll do it by using pattern matching and a little recursion:

```hs
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  
```

There's also a thing called as patterns. Those are a handy way of breaking something up according to a pattern and binding it to names whilst still keeping a reference to the whole thing. You do that by putting a name and an `@` in front of a pattern. For instance, the pattern `xs@(x:y:ys)`. This pattern will match exactly the same thing as x:y:ys but you can easily get the whole list via xs instead of repeating yourself by typing out `x:y:ys` in the function body again. Here's a quick and dirty example:

```hs
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital string@(x:xs) = "The first letter of " ++ string ++ " is " ++ [x]  
```

# Guards, guards!

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

```hs
  
```

```hs
  
```

```hs
  
```
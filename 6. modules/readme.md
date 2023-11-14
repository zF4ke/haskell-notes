# Table of Contents

- [Loading Modules](#loading-modules)
- [Data.List](#datalist)

# Loading modules

A Haskell module is a collection of related functions, types and typeclasses. A Haskell program is a collection of modules where the main module loads up the other modules and then uses the functions defined in them to do something. Having code split up into several modules has quite a lot of advantages. If a module is generic enough, the functions it exports can be used in a multitude of different programs. If your own code is separated into self-contained modules which don't rely on each other too much (we also say they are loosely coupled), you can reuse them later on. It makes the whole deal of writing code more manageable by having it split into several parts, each of which has some sort of purpose.

The Haskell standard library is split into modules, each of them contains functions and types that are somehow related and serve some common purpose. There's a module for manipulating lists, a module for concurrent programming, a module for dealing with complex numbers, etc. All the functions, types and typeclasses that we've dealt with so far were part of the Prelude module, which is imported by default. In this chapter, we're going to examine a few useful modules and the functions that they have. But first, we're going to see how to import modules.

The syntax for importing modules in a Haskell script is `import <module name>`. This must be done before defining any functions, so imports are usually done at the top of the file. One script can, of course, import several modules. Just put each import statement into a separate line. Let's import the Data.List module, which has a bunch of useful functions for working with lists and use a function that it exports to create a function that tells us how many unique elements a list has.

```hs
import Data.List  
    
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub  
```

When you do `import Data.List`, all the functions that `Data.List` exports become available in the global namespace, meaning that you can call them from wherever in the script. `nub` is a function defined in `Data.List` that takes a list and weeds out duplicate elements. Composing length and nub by doing `length . nub` produces a function that's the equivalent of `\xs -> length (nub xs)`.

You can also put the functions of modules into the global namespace when using GHCI. If you're in GHCI and you want to be able to call the functions exported by `Data.List`, do this:

```hs
ghci> :m + Data.List  
```

If we want to load up the names from several modules inside GHCI, we don't have to do :m + several times, we can just load up several modules at once.

```hs
ghci> :m + Data.List Data.Map Data.Set  
```

However, if you've loaded a script that already imports a module, you don't need to use `:m +` to get access to it.

If you just need a couple of functions from a module, you can selectively import just those functions. If we wanted to import only the `nub` and `sort` functions from `Data.List`, we'd do this:

```hs
import Data.List (nub, sort)  
```

You can also choose to import all of the functions of a module except a few select ones. That's often useful when several modules export functions with the same name and you want to get rid of the offending ones. Say we already have our own function that's called `nub` and we want to import all the functions from `Data.List` except the `nub` function: 

```hs
import Data.List hiding (nub)  
```

Another way of dealing with name clashes is to do qualified imports. The `Data.Map` module, which offers a data structure for looking up values by key, exports a bunch of functions with the same name as `Prelude` functions, like `filter` or `null`. So when we import `Data.Map` and then call `filter`, Haskell won't know which function to use. Here's how we solve this:

```hs
import qualified Data.Map  
```

This makes it so that if we want to reference `Data.Map`'s `filter` function, we have to do `Data.Map.filter`, whereas just `filter` still refers to the normal filter we all know and love. But typing out Data.Map in front of every function from that module is kind of tedious. That's why we can rename the qualified import to something shorter:

```hs
import qualified Data.Map as M  
```

Now, to reference `Data.Map`'s `filter` function, we just use `M.filter`.

Use [this handy reference](https://downloads.haskell.org/ghc/latest/docs/libraries/) to see which modules are in the standard library. A great way to pick up new Haskell knowledge is to just click through the standard library reference and explore the modules and their functions. You can also view the Haskell source code for each module. Reading the source code of some modules is a really good way to learn Haskell and get a solid feel for it.

To search for functions or to find out where they're located, use [Hoogle](https://hoogle.haskell.org/). It's a really awesome Haskell search engine, you can search by name, module name or even type signature.

# Data.List

The `Data.List` module is all about lists, obviously. It provides some very useful functions for dealing with them. We've already met some of its functions (like `map` and `filter`) because the `Prelude` module exports some functions from Data.List for convenience. You don't have to import `Data.List` via a qualified import because it doesn't clash with any Prelude names except for those that `Prelude` already steals from `Data.List`. Let's take a look at some of the functions that we haven't met before.

`intersperse` takes an element and a list and then puts that element in between each pair of elements in the list. Here's a demonstration:

```hs
ghci> intersperse '.' "MONKEY"  
"M.O.N.K.E.Y"  
ghci> intersperse 0 [1,2,3,4,5,6]  
[1,0,2,0,3,0,4,0,5,0,6] 
```

`intercalate` takes a list of lists and a list. It then inserts that list in between all those lists and then flattens the result.

```hs
ghci> intercalate " " ["hey","there","guys"]  
"hey there guys"  
ghci> intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]  
[1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]  
```

`transpose` transposes a list of lists. If you look at a list of lists as a 2D matrix, the columns become the rows and vice versa.

```hs
ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]  
[[1,4,7],[2,5,8],[3,6,9]]  
ghci> transpose ["hey","there","guys"]  
["htg","ehu","yey","rs","e"]  
```

`foldl'` and `foldl1'` are stricter versions of their respective lazy incarnations. When using lazy folds on really big lists, you might often get a stack overflow error. The culprit for that is that due to the lazy nature of the folds, the accumulator value isn't actually updated as the folding happens. What actually happens is that the accumulator kind of makes a promise that it will compute its value when asked to actually produce the result (also called a thunk). That happens for every intermediate accumulator and all those thunks overflow your stack. The strict folds aren't lazy buggers and actually compute the intermediate values as they go along instead of filling up your stack with thunks. So if you ever get stack overflow errors when doing lazy folds, try switching to their strict versions.

`concat` flattens a list of lists into just a list of elements.

```hs
ghci> concat ["foo","bar","car"]  
"foobarcar"  
ghci> concat [[3,4,5],[2,3,4],[2,1,1]]  
[3,4,5,2,3,4,2,1,1]  
```

`and` takes a list of boolean values and returns True only if all the values in the list are True.

`or` is like and, only it returns True if any of the boolean values in a list is True.

`any` and `all` take a predicate and then check if any or all the elements in a list satisfy the predicate, respectively. Usually we use these two functions instead of mapping over a list and then doing and or or.

```hs
ghci> any (==4) [2,3,5,6,1,4]  
True  
ghci> all (>4) [6,9,10]  
True 
```

`iterate` takes a function and a starting value. It applies the function to the starting value, then it applies that function to the result, then it applies the function to that result again, etc. It returns all the results in the form of an infinite list.

```hs
ghci> take 10 $ iterate (*2) 1  
[1,2,4,8,16,32,64,128,256,512]  
ghci> take 3 $ iterate (++ "haha") "haha"  
["haha","hahahaha","hahahahahaha"]  
```

`splitAt` takes a number and a list. It then splits the list at that many elements, returning the resulting two lists in a tuple.

```hs
ghci> splitAt 3 "heyman"  
("hey","man")  
ghci> splitAt 100 "heyman"  
("heyman","")  
ghci> splitAt (-3) "heyman"  
("","heyman")  
ghci> let (a,b) = splitAt 3 "foobar" in b ++ a  
"barfoo"  
```

`takeWhile` is a really useful little function. It takes elements from a list while the predicate holds and then when an element is encountered that doesn't satisfy the predicate, it's cut off. It turns out this is very useful.

```hs
ghci> takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]  
[6,5,4]  
ghci> takeWhile (/=' ') "This is a sentence"  
"This"  
```

`dropWhile` is similar, only it drops all the elements while the predicate is true. Once predicate equates to False, it returns the rest of the list. An extremely useful and lovely function!

```hs
ghci> dropWhile (/=' ') "This is a sentence"  
" is a sentence"  
ghci> dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]  
[3,4,5,4,3,2,1]  
```

# Making our own modules

We'll start by creating a file called `Geometry.hs`.

At the beginning of a module, we specify the module name. If we have a file called `Geometry.hs`, then we should name our module `Geometry`. Then, we specify the functions that it exports and after that, we can start writing the functions. So we'll start with this.

```hs
module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where  

sphereVolume :: Float -> Float  
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius ^ 2)  
  
cubeVolume :: Float -> Float  
cubeVolume side = cuboidVolume side side side  
  
cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side  
  
cuboidVolume :: Float -> Float -> Float -> Float  
cuboidVolume a b c = rectangleArea a b * c  
  
cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
  
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b  
```

We also defined a helper function called rectangleArea, which calculates a rectangle's area based on the lenghts of its sides. It's rather trivial because it's just multiplication. Notice that we used it in our functions in the module (namely `cuboidArea` and `cuboidVolume`) but we didn't export it!

To use our module, we just do:

```hs
import Geometry
```

`Geometry.hs` has to be in the same folder that the program that's importing it is in, though.

Modules can also be given a hierarchical structures. Each module can have a number of sub-modules and they can have sub-modules of their own. Let's section these functions off so that `Geometry` is a module that has three sub-modules, one for each type of object.

First, we'll make a **folder** called Geometry. Mind the capital G. In it, we'll place three files: `Sphere.hs`, `Cuboid.hs`, and `Cube.hs`. Here's what the files will contain:

`Sphere.hs`

```hs
module Geometry.Sphere  
( volume  
, area  
) where  
    
volume :: Float -> Float  
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
    
area :: Float -> Float  
area radius = 4 * pi * (radius ^ 2)  
```

`Cuboid.hs`

```hs
module Geometry.Cuboid  
( volume  
, area  
) where  
    
volume :: Float -> Float -> Float -> Float  
volume a b c = rectangleArea a b * c  
    
area :: Float -> Float -> Float -> Float  
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
    
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b  
```

`Cube.hs`

```hs
module Geometry.Cube  
( volume  
, area  
) where  
    
import qualified Geometry.Cuboid as Cuboid  
    
volume :: Float -> Float  
volume side = Cuboid.volume side side side  
    
area :: Float -> Float  
area side = Cuboid.area side side side  
```

Alright! So first is `Geometry.Sphere`. Notice how we placed it in a folder called `Geometry` and then defined the module name as `Geometry.Sphere`. We did the same for the cuboid. Also notice how in all three sub-modules, we defined functions with the same names. We can do this because they're separate modules. We want to use functions from `Geometry.Cuboid` in `Geometry.Cube` but we can't just straight up do import `Geometry.Cuboid` because it exports functions with the same names as `Geometry.Cube`. That's why we do a qualified import and all is well.

```hs
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube  
```

And then we can call `Sphere.area`, `Sphere.volume`, `Cuboid.area`, etc. and each will calculate the area or volume for their corresponding object.

The next time you find yourself writing a file that's really big and has a lot of functions, try to see which functions serve some common purpose and then see if you can put them in their own module. You'll be able to just import your module the next time you're writing a program that requires some of the same functionality.
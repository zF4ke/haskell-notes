# Table of Contents

- [Loading Modules](#loading-modules)

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
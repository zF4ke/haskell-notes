# Table of Contents

- [Hello, world!](#hello-world)
- [Files and streams](#files-and-streams)
- [Command line arguments](#command-line-arguments)
- [Randomness](#randomness)
- [Bytestrings](#bytestrings)
- [Exceptions](#exceptions)

# Hello, world!

Up until now, we've always loaded our functions into GHCI to test them out and play with them. We've also explored the standard library functions that way. But now, after eight or so chapters, we're finally going to write our first *real* Haskell program! Yay! And sure enough, we're going to do the good old `"hello, world"` schtick.

So, for starters, punch in the following in your favorite text editor:

```hs
main = putStrLn "hello, world"
```

We just defined a name called `main` and in it we call a function called `putStrLn` with the parameter `"hello, world"`. Looks pretty much run of the mill, but it isn't, as we'll see in just a few moments. Save that file as `helloworld.hs`.

And now, we're going to do something we've never done before. We're actually going to compile our program! I'm so excited! Open up your terminal and navigate to the directory where `helloworld.hs` is located and do the following:

```hs
$ ghc --make helloworld  
[1 of 1] Compiling Main             ( helloworld.hs, helloworld.o )  
Linking helloworld ...  
```

Okay! With any luck, you got something like this and now you can run your program by doing `./helloworld`.

```hs
$ ./helloworld  
hello, world  
```

And there we go, our first compiled program that printed out something to the terminal. How extraordinarily boring!

Let's examine what we wrote. First, let's look at the type of the function `putStrLn`.

```hs
ghci> :t putStrLn  
putStrLn :: String -> IO ()  
ghci> :t putStrLn "hello, world"  
putStrLn "hello, world" :: IO ()  
```

We can read the type of `putStrLn` like this: `putStrLn` takes a string and returns an **I/O action** that has a result type of `()` (i.e. the empty tuple, also know as unit). An I/O action is something that, when performed, will carry out an action with a side-effect (that's usually either reading from the input or printing stuff to the screen) and will also contain some kind of return value inside it. Printing a string to the terminal doesn't really have any kind of meaningful return value, so a dummy value of `()` is used.

So, when will an I/O action be performed? Well, this is where `main` comes in. An I/O action will be performed when we give it a name of `main` and then run our program.

Having your whole program be just one I/O action seems kind of limiting. That's why we can use do syntax to glue together several I/O actions into one. Take a look at the following example:

```hs
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")  
```

Ah, interesting, new syntax! And this reads pretty much like an imperative program. If you compile it and try it out, it will probably behave just like you expect it to. Notice that we said *do* and then we laid out a series of steps, like we would in an imperative program. Each of these steps is an I/O action. By putting them together with *do* syntax, we glued them into one I/O action. The action that we got has a type of `IO ()`, because that's the type of the last I/O action inside.

Because of that, `main` always has a type signature of `main :: IO something`, where *`something`* is some concrete type. By convention, we don't usually specify a type declaration for `main`.

An interesting thing that we haven't met before is the third line, which states `name <- getLine`. It looks like it reads a line from the input and stores it into a variable called `name`. Does it really? Well, let's examine the type of `getLine`.

Aha, o-kay. `getLine` is an I/O action that contains a result type of `String`. That makes sense, because it will wait for the user to input something at the terminal and then that something will be represented as a string. So what's up with `name <- getLine` then? You can read that piece of code like this: **perform the I/O action** `getLine` **and then bind its result value to** `name`. `getLine` has a type of `IO String`, so name will have a type of String. You can think of an I/O action as a box with little feet that will go out into the real world and do something there (like write some graffiti on a wall) and maybe bring back some data. Once it's fetched that data for you, the only way to open the box and get the data inside it is to use the <- construct. And if we're taking data out of an I/O action, we can only take it out when we're inside another I/O action. This is how Haskell manages to neatly separate the pure and impure parts of our code. getLine is in a sense impure because its result value is not guaranteed to be the same when performed twice. That's why it's sort of tainted with the IO type constructor and we can only get that data out in I/O code. And because I/O code is tainted too, any computation that depends on tainted I/O data will have a tainted result.

When I say *tainted*, I don't mean tainted in such a way that we can never use the result contained in an I/O action ever again in pure code. No, we temporarily *un-taint* the data inside an I/O action when we bind it to a name. When we do `name <- getLine`, name is just a normal string, because it represents what's inside the box. We can have a really complicated function that, say, takes your name (a normal string) as a parameter and tells you your fortune and your whole life's future based on your name. We can do this:

```hs
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name  
```

and `tellFortune` (or any of the functions it passes `name` to) doesn't have to know anything about I/O, it's just a normal `String -> String` function!

Every I/O action that gets performed has a result encapsulated within it. That's why our previous example program could also have been written like this:

```hs
main = do  
    foo <- putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!") 
```

However, `foo` would just have a value of `()`, so doing that would be kind of moot. Notice that we didn't bind the last `putStrLn` to anything. That's because in a do block, **the last action cannot be bound to a name** like the first two were. We'll see exactly why that is so a bit later when we venture off into the world of monads. For now, you can think of it in the way that the do block automatically extracts the value from the last action and binds it to its own result.

Now we're going to make a program that continuously reads a line and prints out the same line with the words reversed. The program's execution will stop when we input a blank line. This is the program:

```hs
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
    
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  
```

> **Protip:** To run a program you can either compile it and then run the produced executable file by doing `ghc --make helloworld` and then `./helloworld` or you can use the runhaskell command like so: `runhaskell helloworld.hs` and your program will be executed on the fly.

First, let's take a look at the `reverseWords` function. It's just a normal function that takes a string like `"hey there man"` and then calls `words` with it to produce a list of words like `["hey","there","man"]`. Then we map `reverse` on the list, getting `["yeh","ereht","nam"]` and then we put that back into one string by using `unwords` and the final result is `"yeh ereht nam"`.

Now what happens when `null line` holds true? What's after the then is performed in that case. If we look up, we'll see that it says `then return ()`. If you've done imperative languages like C, Java or Python, you're probably thinking that you know what this `return` does and chances are you've already skipped this really long paragraph. Well, here's the thing: **the `return` in Haskell is really nothing like the `return` in most other languages!** It has the same name, which confuses a lot of people, but in reality it's quite different. In imperative languages, `return` usually ends the execution of a method or subroutine and makes it report some sort of value to whoever called it. In Haskell (in I/O actions specifically), it makes an I/O action out of a pure value. If you think about the box analogy from before, it takes a value and wraps it up in a box. The resulting I/O action doesn't actually do anything, it just has that value encapsulated as its result. So in an I/O context, `return "haha"` will have a type of IO String. What's the point of just transforming a pure value into an I/O action that doesn't do anything? Why taint our program with `IO` more than it has to be? Well, we needed some I/O action to carry out in the case of an empty input line. That's why we just made a bogus I/O action that doesn't do anything by writing `return ()`.

Using `return` doesn't cause the I/O do block to end in execution or anything like that. For instance, this program will quite happily carry out all the way to the last line:

```hs
main = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line 
```

All these `returns` do is that they make I/O actions that don't really do anything except have an encapsulated result and that result is thrown away because it isn't bound to a name. We can use `return` in combination with `<-` to bind stuff to names.

```hs
main = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b  
```

So you see, `return` is sort of the opposite to `<-`. While `return` takes a value and wraps it up in a box, `<-` takes a box (and performs it) and takes the value out of it, binding it to a name. But doing this is kind of redundant, especially since you can use let bindings in do blocks to bind to names, like so: 

```hs
main = do  
    let a = "hell"  
        b = "yeah"  
    putStrLn $ a ++ " " ++ b  
```

Before we move on to files, let's take a look at some functions that are useful when dealing with I/O.

- **`putStr`** is much like `putStrLn` in that it takes a string as a parameter and returns an I/O action that will print that string to the terminal, only `putStr` doesn't jump into a new line after printing out the string while `putStrLn` does.

```hs
main = do   putStr "Hey, "  
            putStr "I'm "  
            putStrLn "Andy!"
```

```hs
$ runhaskell putstr_test.hs  
Hey, I'm Andy!  
```

- **`putChar`** takes a character and returns an I/O action that will print it out to the terminal.

```hs
main = do   putChar 't'  
            putChar 'e'  
            putChar 'h' 
```

```hs
$ runhaskell putchar_test.hs  
teh  
```

`putStr` is actually defined recursively with the help of `putChar`. The edge condition of `putStr` is the empty string, so if we're printing an empty string, just return an I/O action that does nothing by using `return ()`. If it's not empty, then print the first character of the string by doing `putChar` and then print of them using `putStr`.

```hs
putStr :: String -> IO ()  
putStr [] = return ()  
putStr (x:xs) = do  
    putChar x  
    putStr xs  
```

- **`print`** takes a value of any type that's an instance of `Show` (meaning that we know how to represent it as a string), calls `show` with that value to stringify it and then outputs that string to the terminal. Basically, it's just `putStrLn . show`. It first runs `show` on a value and then feeds that to `putStrLn`, which returns an I/O action that will print out our value.

```hs
main = do   print True  
            print 2  
            print "haha"  
            print 3.2  
            print [3,4,3]  
```
```hs
$ runhaskell print_test.hs  
True  
2  
"haha"  
3.2  
[3,4,3]
```

- **`getChar`** is an I/O action that reads a character from the input. Thus, its type signature is `getChar :: IO Char`, because the result contained within the I/O action is a `Char`. Note that due to buffering, reading of the characters won't actually happen until the user mashes the return key.

```hs
main = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return ()  
```

This program looks like it should read a character and then check if it's a space. If it is, halt execution and if it isn't, print it to the terminal and then do the same thing all over again. Well, it kind of does, only not in the way you might expect. Check this out:

```
$ runhaskell getchar_test.hs  
hello sir  
hello  
```

The `when` function is found in `Control.Monad` (to get access to it, do `import Control.Monad`). It's interesting because in a do block it looks like a control flow statement, but it's actually a normal function. It takes a boolean value and an I/O action if that boolean value is `True`, it returns the same I/O action that we supplied to it. However, if it's `False`, it returns the `return ()`, action, so an I/O action that doesn't do anything. Here's how we could rewrite the previous piece of code with which we demonstrated `getChar` by using `when`:

```hs
import Control.Monad   
    
main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main  
```

So as you can see, it's useful for encapsulating the `if something then do some I/O action else return ()` pattern.

- **`sequence`** takes a list of I/O actions and returns an I/O actions that will perform those actions one after the other. The result contained in that I/O action will be a list of the results of all the I/O actions that were performed. Its type signature is `sequence :: [IO a] -> IO [a]`. Doing this:

```hs
main = do  
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]  
```

Is exactly the same as doing this:.

```hs
main = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs  
```

A common pattern with `sequence` is when we map functions like `print` or `putStrLn` over lists. Doing `map print [1,2,3,4]` won't create an I/O action. It will create a list of I/O actions, because that's like writing `[print 1, print 2, print 3, print 4]`. If we want to transform that list of I/O actions into an I/O action, we have to sequence it.

```hs
ghci> sequence (map print [1,2,3,4,5])  
1  
2  
3  
4  
5  
[(),(),(),(),()]  
```

What's with the `[(),(),(),(),()]` at the end? Well, when we evaluate an I/O action in GHCI, it's performed and then its result is printed out, unless that result is `()`, in which case it's not printed out. But when we do `getLine` in GHCI, the result of that I/O action is printed out, because `getLine` has a type of `IO String`.

Because mapping a function that returns an I/O action over a list and then sequencing it is so common, the utility functions `mapM` and `mapM_` were introduced. `mapM` takes a function and a list, maps the function over the list and then sequences it. `mapM_` does the same, only it throws away the result later. We usually use `mapM_` when we don't care what result our sequenced I/O actions have.

```hs
ghci> mapM print [1,2,3]  
1  
2  
3  
[(),(),()]  
ghci> mapM_ print [1,2,3]  
1  
2  
3  
```

**`forever`** takes an I/O action and returns an I/O action that just repeats the I/O action it got forever. It's located in `Control.Monad`. This little program will indefinitely ask the user for some input and spit it back to him, CAPSLOCKED:

```hs
import Control.Monad  
import Data.Char  
    
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l
```

`forM` (located in `Control.Monad`) is like `mapM`, only that it has its parameters switched around. The first parameter is the list and the second one is the function to map over that list, which is then sequenced. Why is that useful? Well, with some creative use of lambdas and do notation, we can do stuff like this:

```hs
import Control.Monad  
  
main = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors 
```

# Files and streams

Let's meet `getContents`. `getContents` is an I/O action that reads everything from the standard input until it encounters an end-of-file character. Its type is `getContents :: IO String`. What's cool about `getContents` is that it does lazy I/O. When we do `foo <- getContents`, it doesn't read all of the input at once, store it in memory and then bind it to `foo`. No, it's lazy! It'll say: "*Yeah yeah, I'll read the input from the terminal later as we go along, when you really need it!*". 

 Let's make a text file that contains the following little haiku:

 ```hs
I'm a lil' teapot  
What's with that airplane food, huh?  
It's so small, tasteless   
 ```

Now, recall the little program we wrote when we were introducing the `forever` function. It prompted the user for a line, returned it to him in CAPSLOCK and then did that all over again, indefinitely. Just so you don't have to scroll all the way back, here it is again:
 
```hs
import Control.Monad  
import Data.Char  
    
main = forever $ do  
    l <- getLine  
    putStrLn $ map toUpper l  
```

We'll save that program as `capslocker.hs` or something and compile it. And then, we're going to use a unix pipe to feed our text file directly to our little program. We're going to use the help of the GNU cat program, which prints out a file that's given to it as an argument. Check it out, booyaka!

```
$ cat haiku.txt | ./capslocker  
I'M A LIL' TEAPOT  
WHAT'S WITH THAT AIRPLANE FOOD, HUH?  
IT'S SO SMALL, TASTELESS  
capslocker <stdin>: hGetLine: end of file  
```

So what we're essentially doing with that use of forever is taking the input and transforming it into some output. That's why we can use getContents to make our program even shorter and better:

```hs
import Data.Char  
    
main = do  
    contents <- getContents  
    putStr (map toUpper contents)  
```

```
$ cat haiku.txt | ./capslocker  
I'M A LIL' TEAPOT  
WHAT'S WITH THAT AIRPLANE FOOD, HUH?  
IT'S SO SMALL, TASTELESS  
```

Cool, it works. What if we just run capslocker and try to type in the lines ourselves?

```
$ ./capslocker  
hey ho  
HEY HO  
lets go  
LETS GO  
```

We got out of that by pressing Ctrl-D. Pretty nice! As you can see, it prints out our capslocked input back to us line by line.

Let's make program that takes some input and prints out only those lines that are shorter than 10 characters. Observe:

```hs
main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
    
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  
```

This pattern of getting some string from the input, transforming it with a function and then outputting that is so common that there exists a function which makes that even easier, called **`interact`**. `interact` takes a function of type `String -> String` as a parameter and returns an I/O action that will take some input, run that function on it and then print out the function's result. Let's modify our program to use that.

```hs
main = interact shortLinesOnly  
    
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  
```

So far, we've worked with I/O by printing out stuff to the terminal and reading from it. But what about reading and writing files? Well, in a way, we've already been doing that. One way to think about reading from the terminal is to imagine that it's like reading from a (somewhat special) file. Same goes for writing to the terminal, it's kind of like writing to a file. We can call these two files `stdout` and `stdin`, meaning *standard output* and *standard input*, respectively. Keeping that in mind, we'll see that writing to and reading from files is very much like writing to the standard output and reading from the standard input.

We'll start off with a really simple program that opens a file called girlfriend.txt, which contains a verse from Avril Lavigne's #1 hit Girlfriend, and just prints out out to the terminal. Here's girlfriend.txt:

And here's our program:

```hs
import System.IO  
    
main = do  
    handle <- openFile "girlfriend.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  
```

Let's go over the program line by line! Our program is several I/O actions glued together with a do block. In the first line of the do block, we notice a new function called **`openFile`**. This is its type signature: `openFile :: FilePath -> IOMode -> IO Handle`. If you read that out loud, it states: `openFile` takes a file path and an `IOMode` and returns an I/O action that will open a file and have the file's associated handle encapsulated as its result.

`FilePath` is just a type synonym for `String`, simply defined as:

```hs
type FilePath = String  
```

`IOMode` is a type that's defined like this:

```hs
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode  
```

If we bind that action to something we get a `Handle`. A value of type `Handle` represents where our file is. We'll use that handle so we know which file to read from. It would be stupid to read a file but not bind that read to a handle because we wouldn't be able to do anything with the file. So in our case, we bound the handle to `handle`.

In the next line, we see a function called `hGetContents`. It takes a `Handle`, so it knows which file to get the contents from and returns an `IO String` — an I/O action that holds as its result the contents of the file. This function is pretty much like `getContents`. The only difference is that `getContents` will automatically read from the standard input (that is from the terminal), whereas `hGetContents` takes a file handle which tells it which file to read from. In all other respects, they work the same.

With putStr contents we just print the contents out to the standard output and then we do **`hClose`**, which takes a handle and returns an I/O action that closes the file. You have to close the file yourself after opening it with `openFile`!

Another way of doing what we just did is to use the **`withFile`** function, which has a type signature of `withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a`. It takes a path to a file, an `IOMode` and then it takes a function that takes a handle and returns some I/O action. What it returns is an I/O action that will open that file, do something we want with the file and then close it. The result encapsulated in the final I/O action that's returned is the same as the result of the I/O action that the function we give it returns. This might sound a bit complicated, but it's really simple, especially with lambdas, here's our previous example rewritten to use `withFile`:

```hs
import System.IO     
    
main = do     
    withFile "girlfriend.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents)  
```

Here's how we can make our own withFile function:

```hs
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result  
```

Just like we have `hGetContents` that works like `getContents` but for a specific file, there's also **`hGetLine`**, **`hPutStr`**, **`hPutStrLn`**, **`hGetChar`**, etc. They work just like their counterparts without the h, only they take a handle as a parameter and operate on that specific file instead of operating on standard input or standard output. 

- **`readFile`** has a type signature of `readFile :: FilePath -> IO String`. Remember, `FilePath` is just a fancy name for `String`. `readFile` takes a path to a file and returns an I/O action that will read that file (lazily, of course) and bind its contents to something as a string. It's usually more handy than doing `openFile` and binding it to a handle and then doing `hGetContents`. Here's how we could have written our previous example with `readFile`: 

```hs
import System.IO  
    
main = do  
    contents <- readFile "girlfriend.txt"  
    putStr contents  
```

Because we don't get a handle with which to identify our file, we can't close it manually, so Haskell does that for us when we use `readFile`. 

**`writeFile`** has a type of `writeFile :: FilePath -> String -> IO ()`. It takes a path to a file and a string to write to that file and returns an I/O action that will do the writing. If such a file already exists, it will be stomped down to zero length before being written on. Here's how to turn girlfriend.txt into a CAPSLOCKED version and write it to girlfriendcaps.txt: 

```hs
import System.IO     
import Data.Char  
    
main = do     
    contents <- readFile "girlfriend.txt"     
    writeFile "girlfriendcaps.txt" (map toUpper contents)  
```

```
$ runhaskell girlfriendtocaps.hs  
$ cat girlfriendcaps.txt  
HEY! HEY! YOU! YOU!  
I DON'T LIKE YOUR GIRLFRIEND!  
NO WAY! NO WAY!  
I THINK YOU NEED A NEW ONE!  
```

**`appendFile`** has a type signature that's just like `writeFile`, only `appendFile` doesn't truncate the file to zero length if it already exists but it appends stuff to it.

```hs
import System.IO     
    
main = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n")  
```

```
$ runhaskell appendtodo.hs  
Iron the dishes  
$ runhaskell appendtodo.hs  
Dust the dog  
$ runhaskell appendtodo.hs  
Take salad out of the oven  
$ cat todo.txt  
Iron the dishes  
Dust the dog  
Take salad out of the oven  
```

Ooh, one more thing. We talked about how doing `contents <- hGetContents` handle doesn't cause the whole file to be read at once and stored in-memory. It's I/O lazy, so doing this:

```hs
main = do   
    withFile "something.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle  
        putStr contents)  
```

is actually like connecting a pipe from the file to the output. Just like you can think of lists as streams, you can also think of files as streams. This will read one line at a time and print it out to the terminal as it goes along. So you may be asking, how wide is this pipe then? How often will the disk be accessed? Well, for text files, the default buffering is line-buffering usually. That means that the smallest part of the file to be read at once is one line. That's why in this case it actually reads a line, prints it to the output, reads the next line, prints it, etc. For binary files, the default buffering is usually block-buffering. That means that it will read the file chunk by chunk. The chunk size is some size that your operating system thinks is cool.

You can control how exactly buffering is done by using the `hSetBuffering` function. It takes a handle and a `BufferMode` and returns an I/O action that sets the buffering. `BufferMode` is a simple enumeration data type and the possible values it can hold are: `NoBuffering`, `LineBuffering` or `BlockBuffering (Maybe Int)`. The `Maybe Int` is for how big the chunk should be, in bytes. If it's `Nothing`, then the operating system determines the chunk size. `NoBuffering` means that it will be read one character at a time. `NoBuffering` usually sucks as a buffering mode because it has to access the disk so much.

Here's our previous piece of code, only it doesn't read it line by line but reads the whole file in chunks of 2048 bytes.

```hs
main = do   
    withFile "something.txt" ReadMode (\handle -> do  
        hSetBuffering handle $ BlockBuffering (Just 2048)  
        contents <- hGetContents handle  
        putStr contents)  
```

Reading files in bigger chunks can help if we want to minimize disk access or when our file is actually a slow network resource.

We can also use **`hFlush`**, which is a function that takes a handle and returns an I/O action that will flush the buffer of the file associated with the handle. When we're doing line-buffering, the buffer is flushed after every line. When we're doing block-buffering, it's after we've read a chunk. It's also flushed after closing a handle. That means that when we've reached a newline character, the reading (or writing) mechanism reports all the data so far. But we can use `hFlush` to force that reporting of data that has been read so far. After flushing, the data is available to other programs that are running at the same time.

We already made a program to add a new item to our to-do list in todo.txt, now let's make a program to remove an item. I'll just paste the code and then we'll go over the program together so you see that it's really easy. We'll be using a few new functions from `System.Directory` and one new function from `System.IO`, but they'll all be explained.

Anyway, here's the program for removing an item from todo.txt:

```hs
import System.IO  
import System.Directory  
import Data.List  
    
main = do        
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"  
```

At first, we just open todo.txt in read mode and bind its handle to `handle`. 

Next up, we use a function that we haven't met before which is from `System.IO` — **`openTempFile`**. Its name is pretty self-explanatory. It takes a path to a temporary directory and a template name for a file and opens a temporary file. We used `"."` for the temporary directory, because `.` denotes the current directory on just about any OS. We used `"temp"` as the template name for the temporary file, which means that the temporary file will be named temp plus some random characters. It returns an I/O action that makes the temporary file and the result in that I/O action is a pair of values: the name of the temporary file and a handle. We could just open a normal file called todo2.txt or something like that but it's better practice to use `openTempFile` so you know you're probably not overwriting anything. 

The reason we didn't use `getCurrentDirectory` to get the current directory and then pass it to `openTempFile` but instead just passed `"."` to `openTempFile` is because `.` refers to the current directory on unix-like system and Windows 

After that we close both the original and the temporary files and then we remove the original one with **`removeFile`**, which, as you can see, takes a path to a file and deletes it. After deleting the old todo.txt, we use **`renameFile`** to rename the temporary file to todo.txt. Be careful, `removeFile` and `renameFile` (which are both in `System.Directory` by the way) take file paths as their parameters, not handles.

# Command line arguments

Dealing with command line arguments is pretty much a necessity if you want to make a script or application that runs on a terminal. Luckily, Haskell's standard library has a nice way of getting command line arguments of a program.

The System.Environment module has two cool I/O actions. One is `getArgs`, which has a type of `getArgs :: IO [String]` and is an I/O action that will get the arguments that the program was run with and have as its contained result a list with the arguments. `getProgName` has a type of `getProgName :: IO String` and is an I/O action that contains the program name.

Here's a small program that demonstrates how these two work:

```hs
import System.Environment   
import Data.List  
    
main = do  
    args <- getArgs  
    progName <- getProgName  
    putStrLn "The arguments are:"  
    mapM putStrLn args  
    putStrLn "The program name is:"  
    putStrLn progName  
```

# Randomness

Enter the `s` module. It has all the functions that satisfy our need for randomness. Let's just dive into one of the functions it exports then, namely **`random`**. Here's its type: `random :: (RandomGen g, Random a) => g -> (a, g)`. Whoa! Some new typeclasses in this type declaration up in here! The **`RandomGen`** typeclass is for types that can act as sources of randomness. The **`Random`** typeclass is for things that can take on random values. A boolean value can take on a random value, namely `True` or `False`. A number can also take up a plethora of different random values. Can a function take on a random value? I don't think so, probably not! If we try to translate the type declaration of `random` to English, we get something like: it takes a random generator (that's our source of randomness) and returns a random value and a new random generator. Why does it also return a new generator as well as a random value? Well, we'll see in a moment.

To use our random function, we have to get our hands on one of those random generators. The `System.Random` module exports a cool type, namely **`StdGen`** that is an instance of the `RandomGen` typeclass. We can either make a `StdGen` manually or we can tell the system to give us one based on a multitude of sort of random stuff.

To manually make a random generator, use the **`mkStdGen`** function. It has a type of `mkStdGen :: Int -> StdGen`. It takes an integer and based on that, gives us a random generator. Okay then, let's try using `random` and `mkStdGen` in tandem to get a (hardly random) number.

```hs
ghci> random (mkStdGen 100) :: (Int, StdGen)  
(-1352021624,651872571 1655838864)  
```

Finally! A number that looks kind of random! The first component of the tuple is our number whereas the second component is a textual representation of our new random generator. What happens if we call `random` with the same random generator again?

```hs
ghci> random (mkStdGen 100) :: (Int, StdGen)  
(-1352021624,651872571 1655838864)  
```

Of course. The same result for the same parameters. So let's try giving it a different random generator as a parameter.

```hs
ghci> random (mkStdGen 949494) :: (Int, StdGen)  
(539963926,466647808 1655838864)  
```

Let's make a function that simulates tossing a coin three times. If `random` didn't return a new generator along with a random value, we'd have to make this function take three random generators as a parameter and then return coin tosses for each of them. But that sounds wrong because if one generator can make a random value of type `Int` (which can take on a load of different values), it should be able to make three coin tosses (which can take on precisely eight combinations). So this is where `random` returning a new generator along with a value really comes in handy.

We'll represent a coin with a simple `Bool`. `True` is tails, `False` is heads.

```hs
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  
```

Notice that we didn't have to do `random gen :: (Bool, StdGen)`. That's because we already specified that we want booleans in the type declaration of the function. That's why Haskell can infer that we want a boolean value in this case.

So what if we want to flip four coins? Or five? Well, there's a function called **` `** that takes a generator and returns an infinite sequence of values based on that generator.

```hs
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]  
[-1807975507,545074951,-1015194702,-1622477312,-502893664]  
ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]  
[True,True,True,True,False]  
ghci> take 5 $ randoms (mkStdGen 11) :: [Float]  
[7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]  
```

What if we want a random value in some sort of range? All the random integers so far were outrageously big or small. What if we want to to throw a die? Well, we use **`randomR`** for that purpose. It has a type of `randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)`, meaning that it's kind of like random, only it takes as its first parameter a pair of values that set the lower and upper bounds and the final value produced will be within those bounds.

```hs
ghci> randomR (1,6) (mkStdGen 359353)  
(6,1494289578 40692)  
ghci> randomR (1,6) (mkStdGen 35935335)  
(3,1250031057 40692)  
```

There's also **`randomRs`**, which produces a stream of random values within our defined ranges. Check this out:

```hs
ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]  
"ndkxbvmomg"  
```

You may be asking yourself, what does this section have to do with I/O anyway? We haven't done anything concerning I/O so far. Well, so far we've always made our random number generator manually by making it with some arbitrary integer. The problem is, if we do that in our real programs, they will always return the same random numbers, which is no good for us. That's why `System.Random` offers the **`getStdGen`** I/O action, which has a type of `IO StdGen`. When your program starts, it asks the system for a good random number generator and stores that in a so called global generator. `getStdGen` fetches you that global random generator when you bind it to something. 

Here's a simple program that generates a random string.

```hs
import System.Random  
    
main = do  
    gen <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen)  
```

Be careful though, just performing getStdGen twice will ask the system for the same global generator twice. If you do this:

```hs
import System.Random  
    
main = do  
    gen <- getStdGen  
    putStrLn $ take 20 (randomRs ('a','z') gen)  
    gen2 <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen2)  
```

you will get the same string printed out twice! One way to get two different strings of length 20 is to set up an infinite stream and then take the first 20 characters and print them out in one line and then take the second set of 20 characters and print them out in the second line.

Another way is to use the newStdGen action, which splits our current random generator into two generators. It updates the global random generator with one of them and encapsulates the other as its result.

```hs
import System.Random  
    
main = do     
    gen <- getStdGen     
    putStrLn $ take 20 (randomRs ('a','z') gen)     
    gen' <- newStdGen  
    putStr $ take 20 (randomRs ('a','z') gen')     
```

Not only do we get a new random generator when we bind `newStdGen` to something, the global one gets updated as well, so if we do `getStdGen` again and bind it to something, we'll get a generator that's not the same as `gen`.

# Bytestrings

Lists are a cool and useful data structure. So far, we've used them pretty much everywhere. There are a multitude of functions that operate on them and Haskell's laziness allows us to exchange the for and while loops of other languages for filtering and mapping over lists, because evaluation will only happen once it really needs to, so things like infinite lists (and even infinite lists of infinite lists!) are no problem for us. That's why lists can also be used to represent streams, either when reading from the standard input or when reading from files. We can just open a file and read it as a string, even though it will only be accessed when the need arises.

However, processing files as strings has one drawback: it tends to be slow. As you know, `String` is a type synonym for `[Char]`. `Char`s don't have a fixed size, because it takes several bytes to represent a character from, say, Unicode. Furthermore, lists are really lazy. If you have a list like `[1,2,3,4]`, it will be evaluated only when completely necessary. So the whole list is sort of a promise of a list.

That overhead doesn't bother us so much most of the time, but it turns out to be a liability when reading big files and manipulating them. That's why Haskell has **bytestrings**. Bytestrings are sort of like lists, only each element is one byte (or 8 bits) in size. The way they handle laziness is also different. 

Bytestrings come in two flavors: strict and lazy ones. Strict bytestrings reside in `Data.ByteString` and they do away with the laziness completely. There are no promises involved; a strict bytestring represents a series of bytes in an array. You can't have things like infinite strict bytestrings. If you evaluate the first byte of a strict bytestring, you have to evaluate it whole. The upside is that there's less overhead because there are no thunks (the technical term for promise) involved. The downside is that they're likely to fill your memory up faster because they're read into memory at once.

The other variety of bytestrings resides in `Data.ByteString.Lazy`. They're lazy, but not quite as lazy as lists. Like we said before, there are as many thunks in a list as there are elements. That's what makes them kind of slow for some purposes. Lazy bytestrings take a different approach — they are stored in chunks (not to be confused with thunks!), each chunk has a size of 64K. So if you evaluate a byte in a lazy bytestring (by printing it or something), the first 64K will be evaluated. After that, it's just a promise for the rest of the chunks. Lazy bytestrings are kind of like lists of strict bytestrings with a size of 64K. When you process a file with lazy bytestrings, it will be read chunk by chunk. This is cool because it won't cause the memory usage to skyrocket and the 64K probably fits neatly into your CPU's L2 cache.

If you look through the documentation for `Data.ByteString.Lazy`, you'll see that it has a lot of functions that have the same names as the ones from `Data.List`, only the type signatures have `ByteString` instead of `[a]` and `Word8` instead of `a` in them. The functions with the same names mostly act the same as the ones that work on lists. Because the names are the same, we're going to do a qualified import in a script and then load that script into GHCI to play with bytestrings. 

```hs
import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S  
```

The function **`pack`** has the type signature `pack :: [Word8] -> ByteString`. What that means is that it takes a list of bytes of type `Word8` and returns a `ByteString`. You can think of it as taking a list, which is lazy, and making it less lazy, so that it's lazy only at 64K intervals.

What's the deal with that `Word8` type? Well, it's like `Int`, only that it has a much smaller range, namely 0-255. It represents an 8-bit number. And just like `Int`, it's in the `Num` typeclass. For instance, we know that the value `5` is polymorphic in that it can act like any numeral type. Well, it can also take the type of `Word8`.

```hs
ghci> B.pack [99,97,110]  
Chunk "can" Empty  
ghci> B.pack [98..120]  
Chunk "bcdefghijklmnopqrstuvwx" Empty  
```

**`unpack`** is the inverse function of `pack`. It takes a bytestring and turns it into a list of bytes.

**`fromChunks`** takes a list of strict bytestrings and converts it to a lazy bytestring. **`toChunks`** takes a lazy bytestring and converts it to a list of strict ones.

```hs
ghci> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]  
Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))  
```

The bytestring version of `:` is called **`cons`** It takes a byte and a bytestring and puts the byte at the beginning. It's lazy though, so it will make a new chunk even if the first chunk in the bytestring isn't full. That's why it's better to use the strict version of `cons`, **`cons'`** if you're going to be inserting a lot of bytes at the beginning of a bytestring.


```hs
ghci> B.cons 85 $ B.pack [80,81,82,84]  
Chunk "U" (Chunk "PQRT" Empty)  
ghci> B.cons' 85 $ B.pack [80,81,82,84]  
Chunk "UPQRT" Empty  
ghci> foldr B.cons B.empty [50..60]  
Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<"  
Empty))))))))))  
ghci> foldr B.cons' B.empty [50..60]  
Chunk "23456789:;<" Empty  
```

As you can see **`empty`** makes an empty bytestring. See the difference between cons and cons'? With the foldr, we started with an empty bytestring and then went over the list of numbers from the right, adding each number to the beginning of the bytestring. When we used cons, we ended up with one chunk for every byte, which kind of defeats the purpose.

Otherwise, the bytestring modules have a load of functions that are analogous to those in `Data.List`, including, but not limited to, `head`, `tail`, `init`, `null`, `length`, `map`, `reverse`, `foldl`, `foldr`, `concat`, `takeWhile`, `filter`, etc.

It also has functions that have the same name and behave the same as some functions found in `System.IO`, only `String`s are replaced with `ByteString`s. For instance, the readFile function in `System.IO` has a type of `readFile :: FilePath -> IO String`, while the readFile from the bytestring modules has a type of `readFile :: FilePath -> IO ByteString`. Watch out, if you're using strict bytestrings and you attempt to read a file, it will read it into memory at once! With lazy bytestrings, it will read it into neat chunks.

# Exceptions

All languages have procedures, functions, and pieces of code that might fail in some way. That's just a fact of life. Different languages have different ways of handling those failures. In C, we usually use some abnormal return value (like `-1` or a null pointer) to indicate that what a function returned shouldn't be treated like a normal value. Java and C#, on the other hand, tend to use exceptions to handle failure. When an exception is thrown, the control flow jumps to some code that we've defined that does some cleanup and then maybe re-throws the exception so that some other error handling code can take care of some other stuff.

Haskell has a very good type system. Algebraic data types allow for types like `Maybe` and `Either` and we can use values of those types to represent results that may be there or not.

Take a look at this program that opens a file whose name is given to it as a command line argument and tells us how many lines the file has.

```hs
import System.Environment  
import System.IO  
    
main = do (fileName:_) <- getArgs  
            contents <- readFile fileName  
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
```

It works as expected, but what happens when we give it the name of a file that doesn't exist?

```
$ runhaskell linecount.hs i_dont_exist.txt  
linecount.hs: i_dont_exist.txt: openFile: does not exist (No such file or directory)  
```

Aha, we get an error from GHC, telling us that the file does not exist. Our program crashes. What if we wanted to print out a nicer message if the file doesn't exist? One way to do that is to check if the file exists before trying to open it by using the **`doesFileExist`** function from `System.Directory`.

```hs
import System.Environment  
import System.IO  
import System.Directory  
    
main = do (fileName:_) <- getArgs  
            fileExists <- doesFileExist fileName  
            if fileExists  
                then do contents <- readFile fileName  
                        putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
                else do putStrLn "The file doesn't exist!"  
```

Another solution here would be to use exceptions. It's perfectly acceptable to use them in this context. A file not existing is an exception that arises from I/O, so catching it in I/O is fine and dandy.

To deal with this by using exceptions, we're going to take advantage of the **`catch`** function from `System.IO.Error.` Its type is `catch :: IO a -> (IOError -> IO a) -> IO a`. It takes two parameters. The first one is an I/O action. For instance, it could be an I/O action that tries to open a file. The second one is the so-called handler. If the first I/O action passed to `catch` throws an I/O exception, that exception gets passed to the handler, which then decides what to do. So the final result is an I/O action that will either act the same as the first parameter or it will do what the handler tells it if the first I/O action throws an exception.

So let's put our new friend `catch` to use!

```hs
import System.Environment  
import System.IO  
import System.IO.Error  
    
main = toTry `catch` handler  
                
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
            contents <- readFile fileName  
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
    
handler :: IOError -> IO ()  
handler e = putStrLn "Whoops, had some trouble!"  
```

Let's modify our program to catch only the exceptions caused by a file not existing.

```hs
import System.Environment  
import System.IO  
import System.IO.Error  
    
main = toTry `catch` handler  
                
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
            contents <- readFile fileName  
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
    
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e  
```

There are several predicates that act on `IOError` and if a guard doesn't evaluate to `True`, evaluation falls through to the next guard. The predicates that act on `IOError` are:

 - isAlreadyExistsError
 - isDoesNotExistError
 - isAlreadyInUseError
 - isFullError
 - isEOFError
 - isIllegalOperation
 - isPermissionError
 - isUserError

Most of these are pretty self-explanatory. `isUserError` evaluates to `True` when we use the function **`userError`** to make the exception, which is used for making exceptions from our code and equipping them with a string. For instance, you can do `ioError $ userError "remote computer unplugged!"`, although It's preferred you use types like `Either` and `Maybe` to express possible failure instead of throwing exceptions yourself with `userError`.

`System.IO.Error` also exports functions that enable us to ask our exceptions for some attributes, like what the handle of the file that caused the error is, or what the filename is. These start with `ioe` and you can see a [full list of them](www.haskell.org/ghc/docs/6.10.1/html/libraries/base/System-IO-Error.html#3) in the documentation. Say we want to print the filename that caused our error. We can't print the `fileName` that we got from `getArgs`, because only the `IOError` is passed to the handler and the handler doesn't know about anything else. A function depends only on the parameters it was called with. That's why we can use the **`ioeGetFileName`** function, which has a type of `ioeGetFileName :: IOError -> Maybe FilePath`. It takes an `IOError` as a parameter and maybe returns a `FilePath` (which is just a type synonym for `String`, remember, so it's kind of the same thing). Basically, what it does is it extracts the file path from the `IOError`, if it can.

You don't have to use one handler to `catch` exceptions in your whole I/O part. You can just cover certain parts of your I/O code with `catch` or you can cover several of them with `catch` and use different handlers for them, like so:

```hs
main = do toTry `catch` handler1  
            thenTryThis `catch` handler2  
            launchRockets  
```
# Usage of Emacs front-end

## Key bindings

<dl>
<dt>M-C-i</dt>
<dd>Completes a name of keyword, module, class, function, types, language extensions, GHC flags, etc.</dd>
<dt>M-/</dt>
<dd>Completes a name of local symbol.</dd>
<dt>M-t</dt>
<dd>Inserts template. In the beginning of a buffer, "module Foo where" is inserted. On the function without signature, inferred type is inserted. On a symbol `foo` without definition, "foo = undefined" is inserted or a proper module is imported. C-uM-t inserts a hole in this case. On a variable, the case is split. When checking with hlint, original code is replaced with hlint's suggestion if possible.</dd>
<dt>M-C-d</dt>
<dd>Browses the local document with your browser. On a module import line, the document of the module is browsed. On a function or type, its document is browsed.</dd>
<dt>C-uM-C-d</dt>
<dd>Browses the Hackage document of the module with your browser.</dd>
<dt>M-C-m</dt>
<dd>Loads information of symbols for modules in the current buffer. If you add a new line to import a module, type this. The idle timer executes this command anyway.</dd>
<dt>C-cC-h</dt>
<dd>Query somthing at the cursor for hoogle.</dd>
<dt>C-xC-s</dt>
<dd>Saves the buffer if necessary and runs syntax check.</dd>
<dt>C-cC-c</dt>
<dd>Toggle GHC and Hlint for syntax check. GHC is used for initial time.</dd>
<dt>M-n</dt>
<dd>Goes to the next warning or error.</dd>
<dt>M-p</dt>
<dd>Goes to the previous warning or error.</dd>
<dt>M-?</dt>
<dd>Displays the warning/error message in the current line.</dd>
<dt>C-cC-i</dt>
<dd>Displays the info of this expression in another window.</dd>
<dt>C-cC-t</dt>
<dd>Displays the type of this expression in the minibuffer. Type C-cC-t multiple time to enlarge the expression.</dd>
<dt>C-cC-e</dt>
<dd>Displays the expanded Template Haskell.</dd>
<dt>C-cC-m</dt>
<dd>Insert "import Module" for the function at the cursor.</dd>
<dt>M-s</dt>
<dd>Sort and merge import statements in the region.</dd>
<dt>C-cC-j</dt>
<dd>In the beginning of the buffer, errors of other files are displayed. Typing C-cC-j on the errors jumps to the fist file of the error sources.</dd>
<dt>C-cC-k</dt>
<dd>Kill the hppi subprocess for this project. If you install dependency packages, this command should be executed to reflect that change.</dd>
<dt>C-c&lt;</dt>
<dd>Make the indentation of the region shallower.</dd>
<dt>C-c&gt;</dt>
<dd>Make the indentation of the region deeper.</dd>
</dl>

## Resolving import hell

Here is an example story not to type `import Foo` when
you write a Haskell program.

Type the following signature.
Note that `ByteString` can be completed with `M-C-i`.

```
foo :: [ByteString] -> [ByteString] -> [ByteString]
```

`C-xC-s` highlights the signature.
Typing `M-t` on the line results in:

```
import Data.ByteString (ByteString)

foo :: [ByteString] -> [ByteString] -> [ByteString]
foo = undefined
```

Type `M-C-m` to load symbols of `Data.ByteString`.
Then edit the body of `foo`.
Note that `append` can be completed with `M-C-i`.

```
import Data.ByteString (ByteString)

foo :: [ByteString] -> [ByteString] -> [ByteString]
foo bs1 bs2 = B.append <$> bs1 <*> bs2
```

`C-xC-s` highlights the body.
Type `M-t` to get:

```
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

foo :: [ByteString] -> [ByteString] -> [ByteString]
foo bs1 bs2 = B.append <$> bs1 <*> bs2
```

## Type holes

Consider the following broken example:

```
foo :: [a -> a] -> a -> a
foo xs = foldr bar id xs
  where
    bar = (:)
```

`C-xC-s` highlights the 2nd line. `M-?` displays the following:

```
Couldn't match type `[a -> a]' with `a -> a'
Expected type: (a -> a) -> (a -> a) -> a -> a
  Actual type: (a -> a) -> [a -> a] -> [a -> a]
...
```

Even in this situation, `C-cC-t` on `id` displays `a -> a` in the minibuffer. GHC's `-fdefer-type-errors` enables this magic.

Also, you can make use of GHC's hole. Inserting `_` before `bar` and typing `C-xC-s` results in highlighting the second line again:

```
foo :: [a -> a] -> a -> a
foo xs = foldr _bar id xs
  where
    bar = (:)
```

`M-?` displays:

```
Found hole `_bar' with type: (a -> a) -> (a -> a) -> a -> a
Where: `a' is a rigid type variable bound by
           the type signature for foo :: [a -> a] -> a -> a
           at /Users/kazu/c.hs:3:8
Relevant bindings include
  bar :: forall a. a -> [a] -> [a] (bound at /Users/kazu/c.hs:6:5)
  xs :: [a -> a] (bound at /Users/kazu/c.hs:4:5)
  foo :: [a -> a] -> a -> a (bound at /Users/kazu/c.hs:4:1)
...
```

## More type holes

Suppose you have:

```
filterNothing :: [Maybe a] -> [a]
```

Typing `M-t` gets:

```
filterNothing :: [Maybe a] -> [a]
filterNothing = undefined
```

On the other hand, typing `C-uM-t` gets:

```
filterNothing :: [Maybe a] -> [a]
filterNothing xs = _body
```

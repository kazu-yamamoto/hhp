# The `hhpi` command

## Example

```
% hhpi
check Foo.hs
Foo.hs:7:15:Not in scope: `B.append'
OK
bye
NG quit
%
```

## Commands

<dl>
<dt>check &lt;HaskellFile&gt;</dt>
<dd>Checks syntax with GHC</dd>
<dt>find &lt;symbol&gt;</dt>
<dd>Finds all module names exporting &lt;symbol&gt;</dd>
<dt>info &lt;HaskellFile&gt; &lt;expr&gt;</dt>
<dd>Displays information about the expression</dd>
<dt>type &lt;HaskellFile&gt; &lt;line&gt; &lt;column&gt;</dt>
<dd>Displays the types of all expressions including the expression</dd>
<dt>lint [hlint options] &lt;HaskellFile&gt;</dt>
<dd>Checks synstax with Hlint</dd>
<dt>boot</dt>
<dd>Displays boot information for Emacs front-end</dd>
<dt>browse [&lt;package&gt;:]&lt;module&gt;</dt>
<dd>Displays symbols of &lt;module&gt;</dd>
<dt>quit (or empty string)</dt>
<dd>Terminate hhpi
</dl>

## Options

Option should be the form of Haskell's list of `String` (i.e. `[String]`).
Here is an example:

```
lint ["--ignore=Use camelCase", "--ignore=Eta reduce"] Foo.hs
```

## Session separators

<dl>
<dt>OK</dt>
<dd>The session succeeded.</dd>
<dt>NG &lt;error message&gt;</dt>
<dd>The session fails. hhpi gets finished.</dd>
</dl>
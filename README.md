aQuery
===

aQuery is <del>barely-working</del> minimalist symbolic algebra system that can simplify basic expressions, take derivatives, and not much else––all using weird jQuery-style syntax! It also has no error-handling.

You can initialize a mathematical expression using `$`, and then chain other functions to it:

    $( x^2 + sin(x) ).diff(x)
    >> 2x + cos(x)

We can chain lots of things together:

    $(x^2 + 5x - b*x).add(12).diff(x).eval(x=2)
    >> 9 - b

We can evaluate symbolic expressions:

    $(x^2).eval(x=5)
    >> 25

We can simplify expressions:

    $(1*a+0).simplify()
    >> a

We can simplify and evaluate expressions, even if they return symbolic forms:

    $(a*x^2 + b*x + c).eval(x = 1)
    >> a + b + c

Need to debug? Calling `blah.showAST()` will print out the internal binary tree representation of `blah`.

aQuery only supports integers, because other numbers are messy.

"Standard Library"
----

- `$(x)` : parses `x` and initializes a new mathematical expression

- `x.add(y)`: adds expression `y` to `x`
- `x.sub(y)`: subtracts expression `y` from `x`
- `x.mul(y)`: guess!
- `x.div(y)`: guess!
- `x.pow(y)`: raises `x` to the `y`th power

- `x.simplify()`: tries to simplify `x` using some basic mechanical rules
- `x.diff(y)`: differentiates `x` with respect to some variable `y`

- `x.eval(y=z)`: replaces all appearances of `y` in `x` with `z`. (Don't trust it on anything complicated. It works on the syntax tree, not on the math!)

- `x.showAST()`: aQuery doesn't work very well, so if you want to debug, this prints out the syntax tree of `x`

Building
----

Either run it straight from GHC:

    :load main
    main
    welcome to aQuery!
    "quit" to quit.
    aQuery>>

or build it:

    amha$ ghc -o aQuery --make main.hs
    amha$ ./aQuery
    welcome to aQuery!
    "quit" to quit.
    aQuery>>

It requires [Parsec](https://hackage.haskell.org/package/parsec).

Credits
----

I wrote this at the [Recurse Center](http://www.recurse.com) as an exercise in learning Haskell. The REPL code was basically stolen fron [Write Yourself A Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours). 

TODO
----
- add better pretty printer that takes operator priority into account
- add error handling
- make it not evaluate blank lines
-- maybe add a nil value
- use state monad to add a concept of "it"
- sub nothing into eval and calculate as much as you can
- list of things to sub in in eval (this and the former should be straightforward and related)
- try to simplify towards some sort of normal form
- the parser ignores whitespace, but the code to do so is really ugly. (i.e., there's a "spaces" parser tossed in EVERYWHERE.) make this all more beautiful.
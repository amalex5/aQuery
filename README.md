aQuery
===

aQuery is <del>barely-working</del> minimalist symbolic algebra system that can take derivatives, simplify basic expressions, and not much else––all using weird jQuery-style syntax! It also has no error-handling.

You can initialize a mathematical expression using `$`, and then chain other functions to it:

    $( x^2 + sin(x) ).diff(x)
    >> 2x + cos(x)

There are lots of things we can chain:

    $(x^2 + 5x - b*x).add(12).diff(x).eval(x=2)
    >> 9 - b

We can evaluate symbolic expressions:

    $(x^2).eval(x=5)
    >> 25

We can simplify expressions:

    $(1*a+0).simplify()
    >> a

Need to debug? Calling `blah.showAST()` will print out the internal binary tree representation of `blah`.

aQuery only supports integers, because other numbers are messy.

"Standard Library"
----

- `$(x)` : initializes a new mathematical expression

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

It requires [Parsec](https://hackage.haskell.org/package/parsec)


TODO
----
thanks to write you a scheme
and learn you a haskell
and the parsec docs
for all the code!

- make it not evaluate blank lines
-- maybe add a nil value
- teach it how to distribute 
- use state monad to add a concept of "it"
sub nothing into eval and calculate as much as you can
list of things to sub in in eval
distribute multiplication

DONE:
- the parser ignores whitespace, but the code to do so is really ugly. (i.e., there's a "spaces" parser tossed in EVERYWHERE.)
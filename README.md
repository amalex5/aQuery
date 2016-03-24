aQuery
===

aQuery is barely-working symbolic algebra system that can take derivatives, simplify basic expressions, and not much else! It has no error-handling! It has weird jQuery style syntax!

You can initialize a mathematical expression using `$`, and then chain other functions to it:

    $[x^2 + sin(x)].diff[x]
    >> 2x + cos(x)

There are lots of things we can chain:

    $[x^2].add[sin(x)].mul[5]
    >> x^2+sin(x)*5

Um, I guarantee you that in the internal state of the program, it actually looks correct: like `5 * (x^2 + sin(x))`. I promise! Here, we can check by evaluating it for `x=2`:

    $[x^2].add[sin(x)].mul[5].eval[x=2]
    >> 5*4+sin(2)

You can evaluate symbolic expressions:

    $[x^2].eval[x=5]
    >> 25

We can simplify expressions:

    $[1*a+0].simplify[]
    >> a

We can, as we're typing, decide to chain 


`eval` is actually really powerful, since it just substitutes any appearances of the right-hand side of the equation for anything on the left-hand side:

    $[a+b+c].eval[a+b=sin(x)-2]  
    >> sin(x)-2+c

But this doesn't always work, since it operates at the level of the tree-like datatype for these expressions, which isn't really the same as the math:

    $[a+b+c].eval[b+c=5]
    >> a+b+c

In this case, `$[a+b+c]` is represented internally, more or less, as `Add (Add a b) c`. But `eval[b+c=5]` looks for `Add b c`, and not finding it, makes no replacements.


here's another example of trees failing:
    $[x+b+x].eval[x=7]
    >> 7+b+7

Again, this is represented internall by something like `(+ (+ 7 b) 7)`. 

$[1*a + 0].simplify[]
>> a

$[x^5].add[x^7]
>> x^5 + x^7

$[a+b*c].eval[b*c=j]
>> "a+j"

you can evaluate

also, it only supports integers.

thanks to write you a scheme
and learn you a haskell
and the parsec docs
for all the code!


TODO: 
- teach it how to distribute 
- use state monad to add a concept of "it"
sub nothing into eval and calculate as much as you can
list of things to sub in in eval
distribute multiplication

DONE:
- the parser ignores whitespace, but the code to do so is really ugly. (i.e., there's a "spaces" parser tossed in EVERYWHERE.)
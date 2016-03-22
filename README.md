
A barely-working symbolic algebra system that can take derivatives, simplify basic expressions, and not much else! No error-handling! Weird jQuery style syntax!

$[x^2 + sin(x)].diff[x]
>> 2x + cos(x)

$[1*a + 0].simplify[]
>> a

$[x^5].add[x^7]
>> x^5 + x^7


you can evaluate

also, it only supports integers.

TODO: 
sub nothing into eval and calculate as much as you can
list of things to sub in in eval
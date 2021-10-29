
Plan:
    - Chapter 5: lambda calculus. skip the church encoding stuff. The moral is that lambda calculus can express any computation even though its only built-in data type is the function.
    - Chapter 6 skip/skim: The moral is that variable binding and substitution are tricky to get right and there are techniques for handling them safely. We can revisit this chapter later when/if we get to higher-kinded types or polymorphism
    - recommended exercises:
        - easy (1 star) and medium (2 star) exercises, skipping exercises to do with church encoding

./church.ml has some of the church encoding stuff 


p72.
5.3.6
Adapt these call-by-value rules for full beta-reduction, normal order, and call-by-name

Defined on p56-57:
- call by value: only outermost redexes are reduced and where redex is reduced only when its right-hand-side has already been reduced to a value.
- Full beta-reduction: any redex can be reduced at any time
- Normal Order: leftmost, outermost redex is reduced first
- call by name (lazy): leftmost, outermost redex is reduced first and no reductions allowed inside abstractions 


call by value (given)

```
t1 -> t1'
----------           E-App1
t1 t2 -> t1't2

t2 -> t2'
---------------      E-App2
v1 t2 -> v1 t2'

(\x.t12) v2 -> [x |-> v2]t12    E-AppAbs
```

call by name
> similar to call by value, but
> don't need arguments to be values
```
t1 -> t1'
---------------      E-App1
t1 t2 -> t1' t2

(\x.t12) t2 -> [x |-> t2]t12    E-AppAbs
```

full beta reduction

> reduce everything, regardless of whether it is a value
```
t1 -> t1'
----------           E-App1
t1 t2  -> t1' t2

t2 -> t2'
---------------      E-App2
t1 t2 -> t1 t2'

(\x.t12) t2 -> [x |-> t2]t12    E-AppAbs
```


normal order
> like full beta reduction, but left-most first
> I got this wrong at first and adapted from the appendix

```
terms

t ::= ... as defined for cbv

nf ::=
    | \x. nf
    | nanf

nanf ::=
    | x
    | nf

na ::=
    | x
    | t1 t2


eval

na -> na'
----------           E-App1
na t2  -> na' t2

t2 -> t2'
---------------      E-App2
nanf t2 -> nanf t2'

(\x.t12) t2 -> [x |-> t2]t12    E-AppAbs
```


p73
5.3.8


> Difference between my answer and the one in the appendix is
> that in mine the result has to be a value. Why does the appendix
> just use a term?
```
t1 => \x.t12
t2 => v2
[x |-> v2]t12  => v 
------------ E-App
t1 t2 => v

```



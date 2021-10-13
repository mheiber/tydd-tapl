
Chapter 1

page 3


```
if <complex test>:
    return 5
else:
    // incorrect types:
    return true * 5
```





Chapter 3: simple arithmetic expressions


p27
3.2.4**
how many elements in S3

```
s_0 = \empty
s_1 = {true, false, 0}     // 3
         \union {succ \empty, pred \empty, iszero \empty} // 3
         \union {if \empty then \empty else empty}       // 1
S_2 =
        {true, false, 0}
         \union {succ \empty, pred \empty, iszero \empty} // 3
             \union {succ (succ \empty), succ (pred \empty), succ (iszero \empty)}
             \union {pred (succ \empty), pred (pred \empty), pred (iszero \empty)}
             \union {iszero (succ \empty), iszero (pred \empty), iszero (iszero \empty)}
         \union {if \empty then \empty else empty}       // 1
             \union {if (7 options) then (7 options) else (7 options)}


|S_i| for i >= 1 = 3          // true, false, 0
       + (size_of_prev * 3)   // succ, pred, iszero
       + (size_of_prev)^3     // if

|S_1| = 3
|S_2| = 3^3 + 3 * 3 +  3 = 39
|S_2| = 39^3 + 3 * 39 + 3 = 59439

```

3.2.5

Demonstrating on a smaller example

S_0 = \empty
s_{i + 1} = {true} \union {succ t_1 | t_1 \mem S_i}

Using strong induction.

```
case i = 0: P(i) for all i < 0 (trivial)
case i = succ i':
    show forall m' in S_{i'}, m' in S_i
    induct on all m in S_i, show that if m is in S_i then m is in S_i':
        case m = true:
            true is in S_i and S_i'
        case m = Succ n:
            by induction hypothesis n is in S_i and S_i'
```



%
p31 3.3.4

depth induction: induction on natural numbers where n = depth(term)
size induction: induction on natural numbers where n = size(term)
structural induction: EITHER
- natural number induction where n = the order in which term would be reached in depth-first search through the expression tree
- OR lexicographic induction where n = depth and m is such that term is an mth child

p39
3.5.10

```

----reflexivity
t -> t'

t -> t'
----closure
t ->* t'

t -> t'
t' -> t''
----transitivity
t ->* t''
```


%
p40
3.5.13

EFunny1 rule

- 3.5.4 (determinacy): invalid because multiple rules apply in `if true ...`
- 3.5.7 (every value is in a normal form): valid
- 3.5.8 (every t in normal form is a value): valid
- 3.5.11 (uniqueness of normal form): invalid. counterexample

0
------------------------- EIfTrue
if true then 0 else succ 0



succ 0
------------------------- EFunny1
if true then 0 else succ 0




- 3.5.12 (termination): valid

EFunny2 rule

determinacy: invalid
every value in normal form: valid
every t in normal form is a value: valid
3.5.11 uniqueness of normal form: valid

prove that any applications of the rules lead to the same normal form

Case analysis on
`if t1 then t2 else t3`

Induction hypothesis: determinacy of evaluation. So we only need to show that
we can evaluate to the same expression regardless of whether we choose to apply
EFunny2 or another rule.

suppose EIFTrue and EFunny2 both apply.
(Note: Since EIFTrue applies t1 must be true)

```
t2'
-------------
t2       t2 --> t2'
------EIFTrue
if true then t2 else t3
```

```
t2'
--------------ETrue
if true then t2' else t3
------EFunny2
if true then t2 else t3 and t2 --> t2'
```

if EIFFalse applies then we end up at t3 by similar reasoning

if Eif and then ETrue apply:

```
t2'
----by assumption
t2
------ETrue
if true then t2 else t3
------EIF
if t1 then t2 else t3 and t1 --> true
```

```
t2'
--------------ETrue
if t1 then t2' else t3
------EFunny2
if t1 then t2 else t3 and t2 --> t2'
```

if Eif and then EFalse apply:

```
t3
------EFalse
if false then t2 else t3
------EIF
if t1 then t2 else t3 and t1 --> true
```

```
t3
--------------EFalse
if t1 then t2' else t3
------EFunny2
if t1 then t2 else t3 and t2 --> t2'
```


%
BONUS questions:
- Give an expression that doesn't have a normal form. Does this have anything to do with type-checking?
- Does adding EFunny2 to the system affect which expressions have normal forms?



p41
3.5.14 (boring)

show that if t -> t' and t -> t'' then t = t''

induct on derivation t -> t'

if the rule is ESucc then only E-Succ can apply because no other derivation has `succ` as its outermost constructor.

If the rule is EPredZero then t has the form `pred 0` so no other rules apply: in particular, EPredSucc only applies to `pred (succ nv1')`

.......



%
p43
3.5.18

Small step:

```

t2 -> t2'
------------------------
if t1 then t2 else t3 -> if t1 then t2' else t3


t3 -> t3'
-------------
if t1 then v2 else t3 -> if t1 then v2 else t3'

t1 -> t1'
--------------
if t1 then v2 else v3 -> if t1' then v2 else v3

```

Big step:
(does this really express order of evaluation?)

```
t2 => v2
t3 => v3
t1 => true
------------------------(B-IfTrue)
if t1 then t2 else t3 => v2

t2 => v2
t3 => v3
t1 => false
------------------------(B-IfFalse)
if t1 then t2 else t3 => v3
```


4.2.2 ./big_step.ml

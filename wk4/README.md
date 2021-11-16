> note: I was really short on time


**TO DISCUSS** soundness

p94 8.2.3
Every sub-term of a well-typed term is well-typed.

By induction on the typing judgments:
in each rule that mentions terms with sub-terms, the assumption is that those sub-terms are well-typed.

p98
8.3.4
skip lazy

8.3.5
With this changee, evaluating `pred 0` produes a runtime error, which would mean we need to give up on soundness or tweak our definition of soundness. This case is similar to division by zero, which in some "sound"
type systems type-checks but leads to a runtime error.

**8.3.6**
subject expansion
good question!



8.3.7
fat arrow instead of skinny arrow?

8.3.8
type safety = well-typed terms don't evaluate to `wrong`

p102
9.2.1

In STLC with no base types, every type is 
an arrow type t1 -> t2. These form an infinite
binary tree.
Take any node, t1 -> t2. Given that each type
is an arrow type, t1 has sub-terms t1' and t1'' and t2 has sub-terms t2' and t2''.

p103
9.2.2
TODO (easy)

9.2.3
gamma = 
f: Unit -> Bool,
x: Unit

For all alpha: Type, there is a case like this:
gamma =
f: alpha -> Bool
x: alpha

**p104**
9.3.2

no such case
by induction on the depth of x
forall gamma where gamma |- alpha beta : T,
the depth of alpha must be (1 + depth of beta), based on the typing rule for fun application.

So there is no gamma |- x x : T, since
x cannot have depth 1 + x

9.3.3
not sure what to say. Induction

**p108** TODO
9.4.1
fig 8.1 on p93
intro rules:

elim rules:


fig 8.2 on p93
intro rules:

elim rules:

**TO DISCUSS** church vs curry 9.6 p111


Chapter 11

see ./check.ml

11.11.1 define equal, plus, times, factorial using fix: skip

11.11.2 skip

11.12.1 progress and preservation for lists

11.12.2 can all type annotations be deleted.
Good question! Probably, with H-M inference.
I haven't proven it.

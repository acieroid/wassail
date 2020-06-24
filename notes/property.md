Which property do we want to tackle?

It needs to be something that can be checked compositionally.

# Input-output relation
That's the "easiest": establish a relation between the input and output of each procedure.
This is something we already support, as this is the core concept of summary.

How can we measure this is correct? 

# Taint propagation
On top of the summary, we may need to track may-memory.
For now, we have "must memory": M[x: y] joined with M is M, meaning that x can be bound to anything
With may memory, we would start with [], and [] joined with [x: y] would be [x: y], meaning that x may be bound to y.

This may be tricky and will probably require some backwards propagation!

Consider the current analysis with only a "must store":

```
;; Step 1: load what resides in memory location 1024
;; vstack: [], must store: [], constraints: <>
i32.const 1024
;; vstack: [x], must store: [], constraints: <x = 1204>
i32.load
;; vstack: [y], must store: [], <x = 1024> ;; <- no restriction on y

;; Step 2: stores it at location 1024
i32.const 1028
;; vstack: [z, y], must store: [], <y = 1028, x = 1024>
i32.store
;; vstack: [], must store: [y: z], <y = 1028, x = 1024>
```

**The problem**: taint is propagated from 1024 to z, but there is no information in the resulting summary (`M -> M[y: z] with <x = 1024, y = 1028>`).
We would need to have `M[x: w][y: z] with <x = 1024, y = 1028, w = z>`

## Solution 1: assume input store
We can instead compute the input store as we go forward:

```
;; vstack: [], must store: [], constraints: <>
i32.const 1024
;; vstack: [x], must store: [], constraints: <x = 1204>
i32.load
;; ASSUME that [x: w] is bound in the input store
;; the result changes:
;; vstack: [w], must store: [x: w], <x = 1024, w = Top> ;;

;; Step 2: stores it at location 1024
i32.const 1028
;; vstack: [z, w], must store: [x: w], <y = 1028, x = 1024, w = Top>
i32.store
;; vstack: [], must store: [x: w][y: w], <y = 1028, x = 1024, w = Top>
```

Because there have been new assumptions, we have to start the analysis again:
```
;; vstack: [], must store: [x: w], constraints: <>
i32.const 1024
;; vstack: [x], must store: [x: w], constraints: <x = 1204>
i32.load
;; vstack: [w], must store: [x: w], <x = 1024, w = Top> ;;

;; Step 2: stores it at location 1024
i32.const 1028
;; vstack: [z, w], must store: [x: w], <y = 1028, x = 1024, w = Top>
i32.store
;; vstack: [], must store: [x: w][y: w], <y = 1028, x = 1024, w = Top>
```

### Joining
An important question is how to join:
 - **must store**: `[x: a][y: b]` joined with `[x: a]` in a must store results in `[x: a]`, because it is the only address that clearly must be bound. This is the "most general store". Also, joining `[x: a]` with `[x: b]` results in [].
 - **may store**: in a may store, it should result in `[x: a][y: b]`. This is the "most specific store". As a result, joining `[x: a]` with `[x: b]` results in `[x: {a,b}]`

Do we need two stores then? How does load/store work then?

### If
```
local.get 0
;; vstack: [p0], must store: [], may store: [], constraints: <>
if
  i32.const 1024
  ;; vstack: [x], constraints: <x = 1024>
  i32.load
  ;; assumption: store should have contained [x: a]
  ;; vstack: [res1], must store: [x: a], may store: [x: a], constraints: <x = 1024, res1 = a>
else
  i32.const 1028
  ;; vstack: [y], constraints: <y = 1024>
  i32.load
  ;; assumption: store should have contained [y: b]
  ;; vstack: [res2], must store: [y: b], may store: [y: b], constraints: <y = 1028, res2 = b>
end
;; results: must store: [], may store: [x: a][y: b]
;; what happens to the constraints upon join? they are top because every variable is constrained only on one branch.
```

Here we clearly need the summary to state that the resulting value may be tainted with a or with b.
Do we need a vstack for taint? Where the information is joined as union.

### If, second try:
```
local.get 0
;; vstack: [p0], must store: [], may store: [], constraints: <>
;; vstack taint: [p0]
if
  i32.const 1024
  ;; vstack: [x], constraints: <x = 1024>
  ;; vstack taint: [_]
  i32.load
  ;; assumption: store should have contained [x: a]
  ;; vstack: [res1], must store: [x: a], may store: [x: a], constraints: <x = 1024, res1 = a>
  ;; vstack taint: [a]
else
  i32.const 1028
  ;; vstack: [y], constraints: <y = 1024>
  i32.load
  ;; assumption: store should have contained [y: b]
  ;; vstack: [res2], must store: [y: b], may store: [y: b], constraints: <y = 1028, res2 = b>
  ;; vstack taint: [b]
end
;; results: must store: [], may store: [x: a][y: b]
;; vstack taint: [{a,b}]
```

That's better, but another problem now is that x and y are unconstrained due to the join on the constraints.
Maybe we need the meet as well: we'd have the may store as we want, the vstack taint as we want, and we'd know that `<x = 1024, y = 1028>`


### If, third try
To check this, let's try not with 1024 but now with actual params

```
local.get 0
;; vstack: [p0], must store: [], may store: [], constraints: <>
;; vstack taint: [p0]
if
  local.get 0
  ;; vstack: [x], constraints: <x = p0>
  ;; vstack taint: [p0]
  i32.load
  ;; assumption: store should have contained [x: a]
  ;; vstack: [res1], must store: [x: a], may store: [x: a], constraints: <x = 1024, res1 = a>
  ;; vstack taint: [a]
else
  local.get 1
  ;; vstack: [y], constraints: <y = p1>
  i32.load
  ;; assumption: store should have contained [y: b]
  ;; vstack: [res2], must store: [y: b], may store: [y: b], constraints: <y = 1028, res2 = b>
  ;; vstack taint: [b]
end
;; results: must store: [], may store: [x: a][y: b]
;; vstack taint: [{a,b}]
;; constraints meet: <x = p0, y = p1>
```

This seems to be what we need.
So, four extra things on top of the base analysis:
  - a may store on top of the must store -> this is really just a taint store!
  - assumptions on the store (the may store is just a representation of the initial store! It should not model store operations) -> no, we just need to prebuild the store based on store/load operations!
  - the meet of constraints on top of their join
  - a track of the taint on top of the vstack
  

### If + store
```
local.get 0
if
  local.get 0
  i32.load
  local.get 1
  i32.store
  ;; stores p0 in p1
else
  local.get 1
  i32.load
  local.get 0
  i32.store
  ;; stores p1 in p0
end
```

The expected result is that location p1 becomes tainted with p0, and vice versa.

```
local.get 0
;; vstack: [p0]
;; must store: []
;; may store: []
;; constraints: <>
if
  local.get 0
  ;; vstack: [p0]
  i32.load
  ;; vstack: [x]
  ;; ASSUME p0: x in the input store
  ;; x unconstrained
  ;; must store: [p0: x]
  ;; may store: [p0: x]
  local.get 1
  ;; vstack: [p1, x]
  i32.store
  ;; vstack: []
  ;; must store: [p0: x, p1: x]
  ;; may store: [p0: x, p1: x]
else
  local.get 1
  i32.load
  ;; ASSUME: p1: y in the input store
  local.get 0
  i32.store
  ;; stores p1 in p0
  ;; must store: [p1: y, p0: y]
  ;; may store: [p1: y, p0: y]
end
;; vstack: []
;; must store: []
;; may store: [p0: {x,y}, p1: {x,y}] 
;; assumptions: input store is [p0: x, p1: y]
```

Hence, the summary is, in separation logic style:
```
p0 -> x * p1 -> y

-->

p0 -> {x,y} * p1 -> {x,y}
```

Meaning that if the value residing at p0 (x) is tainted, then after the facts, both p0 and p1 may be tainted.

### loop

Iteration 1:
```
block
  loop
    ;; This is a merge point
    ;; check if l2 = 0
    local.get 2
    i32.eqz
    br_if 1 ;; break out of the block

    ;; we have l2 > 0

    ;; copy *l0 into *l1
    local.get 0
    ;; vstack: [i0]
    ;; contraints: <i0 = l0>
    i32.load
    ;; ASSUME [l0: x]
    ;; vstack: [y]
    ;; constraints: <y = x>
    ;; store: [l0: x]
    local.get 1
    ;; vstack: [l1, y]
    i32.store
    ;; store: [l1: y, l0: x]
    ;; constraints: <y = x>

    ;; increase l0
    local.get 0
    ;; vstack: [l0]
    i32.const 1
    ;; vstack: [a, l0]
    i32.add
    ;; vstack: [b]
    local.set 0
    ;; vstack: []
    ;; locals: [c, l1, l2]
    ;; store: [l1: y, l0: x]
    ;; constraints: <c=b, b=l0+a, a=1>


    ;; increase l1
    local.get 1
    ;; vstack: [l1]
    i32.const 1
    ;; vstack: [d, l1]
    i32.add
    ;; vstack: [e]
    local.set 0
    ;; locals: [c, f, l2]
    ;; store: [l1: y, l0: x]
    ;; constraints: <c=b, b=l0+a, a=1, f=e, e=d+l1, d=1>

    ;; decrease l2
    local.get l2
    ;; vstack: [l2]
    i32.const 1
    ;; vstack: [g, l2]
    i32.sub
    ;; vstack: [h]
    local.set l2
    ;; vstack: []
    ;; locals: [c, d, i]
    ;; store: [l1: y, l0: x]
    ;; constraints: <c=b, b=l0+a, a=1,
    ;;               f=e, e=d+l1, d=1,
    ;;               i=h, h=l2-g, g=1>

    ;; loops
    br 0
```

Ideal
vstack: []
must store: []
may store: [x: a, y: a]
input store: [x: a, y: b]
constraints: x >= l0, x <= l0 + l2, y >= l1, y <= l1 + l2

Why two stores if the same result can directly be achieved by joining as follows?
[x: a] joined with [y: b] becomes
[x: a', y: b'] where a' is bound to top, b' = top
Or is it not the same result?




Iteration 2:
```
block
  loop
    ;; check if l2 = 0
    local.get 2
    i32.eqz
    br_if 1 ;; break out of the block

    ;; we have l2 > 0
    ;; vstack: []
    ;; locals: [c, d, i]
    ;; store: [l1: y, l0: x]
    ;; constraints: <c=b, b=l0+a, a=1,
    ;;               f=e, e=d+l1, d=1,
    ;;               i=h, h=l2-g, g=1>

    ;; copy *l0 into *l1
    local.get 0
    ;; vstack: [l0]
    ;; contraints: <c=b, b=l0+a, a=1,
    ;;              f=e, e=d+l1, d=1,
    ;;              i=h, h=l2-g, g=1,
    ;;              l0=c>
    ;; store: [l1: y, l0: x]
    i32.load
    ;; vstack: [y]
    ;; constraints: <y = x>
    ;; store: [l0: x]
    local.get 1
    ;; vstack: [l1, y]
    i32.store
    ;; store: [l1: y, l0: x]
    ;; constraints: <y = x>

    ;; increase l0
    local.get 0
    ;; vstack: [l0]
    i32.const 1
    ;; vstack: [a, l0]
    i32.add
    ;; vstack: [b]
    local.set 0
    ;; vstack: []
    ;; locals: [c, l1, l2]
    ;; store: [l1: y, l0: x]
    ;; constraints: <c=b, b=l0+a, a=1>


    ;; increase l1
    local.get 1
    ;; vstack: [l1]
    i32.const 1
    ;; vstack: [d, l1]
    i32.add
    ;; vstack: [e]
    local.set 0
    ;; locals: [c, f, l2]
    ;; store: [l1: y, l0: x]
    ;; constraints: <c=b, b=l0+a, a=1, f=e, e=d+l1, d=1>

    ;; decrease l2
    local.get l2
    ;; vstack: [l2]
    i32.const 1
    ;; vstack: [g, l2]
    i32.sub
    ;; vstack: [h]
    local.set l2
    ;; vstack: []
    ;; locals: [c, d, i]
    ;; store: [l1: y, l0: x]
    ;; constraints: <c=b, b=l0+a, a=1,
    ;;               f=e, e=d+l1, d=1,
    ;;               i=h, h=l2-g, g=1>

    ;; loops
    br 0
```

## Solution 1, second try: prebuild the store
We can prebuild the store based on when load operations appear.
Also of important note, the vstack discipline does not matter for the analysis, as everything is encoded in the vstack spec: we only need to care about the constraints.
Probably the same for locals/globals/store !

### Simple example
```
i32.const 1024 ;; vstack spec: [i0]
i32.load ;; vstack spec: [i1]
i32.const 1028 ;; vstack spec: [i2,i1]
i32.store ;; vstack spec: []
```

The prebuilt store is then: `[i0: m0][i2: m1]`
(offsets are static and should be taken into account as well, but that's for later)

The final specification is therefore
```
;; vstack spec: []
;; mem spec: [i0: m0][i2: m1]
i32.const 1024
;; vstack spec: [i0]
i32.load
;; vstack spec: [i1]
i32.const 1028
 ;; vstack: [i2,i1]
i32.store
;; vstack spec: []
;; mem spec: [i0: m0][i2: m2]
```

We can then run the analysis:

```
;; constraints: <>
i32.const 1024
;; new constraint: <i0 = 1024>
i32.load
;; new constraint: <i1 = m0>
i32.const 1028
;; new constraint: <i2 = 1028>
i32.store
;; new constraint: <m2 = i1>
```
As a result, we know the store is `[i0: m0][i2: m2]`, with constraints (solved by hand)
`m2 = m0, i0 = 1024, i2 = 1028`
This is precisely what we need.

### If
```
;; vstack spec: []
local.get 0
;; vstack spec: [i0]
if
  ;; vstack spec: []
  i32.const 1024
  ;; vstack spec: [i1]
  i32.load
  ;; vstack spec: [i3]
else
  ;; vstack spec: ]]
  i32.const 1028
  ;; vstack spec: [i4]
  i32.load
  ;; vstack spec: [i5]
end
;; vstack spec: [i6]
```

We can build the memory backwards, joining results as union, and we get
`[i1: m0][i4: m1]`

Then we can annotate with memory spec:

```
;; vstack spec: []
;; memory spec: [i1: m0][i4: m1]
local.get 0
... (same)

```

Finally, we run the analysis:
```
;; vstack spec: []
local.get 0
;; vstack spec: [i0]
;; new constraint: <i0 = l0>
if
  ;; vstack spec: []
  i32.const 1024
  ;; vstack spec: [i1]
  ;; new constraint: <i1 = 1024>
  i32.load
  ;; vstack spec: [i3]
  ;; new constraint: <i3 = m0>
else
  ;; vstack spec: []
  i32.const 1028
  ;; vstack spec: [i4]
  ;; new constraint: <i1 = 1028>
  i32.load
  ;; vstack spec: [i5]
  ;; new constraint: <i5 = m1>
end
;; vstack spec: [i6]
```
but the constraints for i6 are lost in joining.
Maybe we do need a vstack for taint: clearly it can be joined easily in this case.

A twist on the same example is to use the store:

```
;; Memory is: [i2: m0][i1: m1][i5: m3][i4: m4]
;; vstack spec: []
local.get 0
;; vstack spec: [i0]
;; new constraint: <i0 = l0>
if
  i32.const 2048
  ;; vstack spec: [i1]
  i32.const 1024
  ;; vstack spec: [i2, i1]
  ;; new constraint: <i2 = 1024>
  i32.load
  ;; vstack spec: [i3, i1]
  ;; new constraint: <i3 = m0>
  i32.store
  ;; vstack spec: []
  ;; store spec: [i2: m0][i1: m2][i5: m3][i4: m5]
  ;; new constraint: m2 = i3
else
  ;; store spec: [i2: m0][i1: m1][i5: m3][i4: m4]
  ;; vstack spec: []
  i32.const 2048
  ;; vstack spec: [i4]
  ;; new constraint: <i4 = 1028>
  i32.const 1028
  ;; vstack spec: [i5, i4]
  ;; new constraint: <i5 = 1028>
  i32.load
  ;; vstack spec: [i6, i4]
  ;; new constraint: <i6 = m4>
  i32.store
  ;; vstack spec: []
  ;; store spec: [i2: m0][i1: m1][i5: m3][i4: m5]
end
;; vstack spec: []
i32.const 2048
i32.load
```

This will have the same limitation: m4 and m5 will be merge in the same variable mX, which will be bound to top and we lost our information.

Do we need a domain where the store maps to set of variables?

```
if
  ;; vstack spec: []
  i32.const 1024
  ;; vstack spec: [i1]
  ;; new constraint: <i1 = 1024>
  local.get 0
  ;; vstack spec: [i2, i1]
  ;; new constraint: <i2 = l0>
  i32.store
  ;; store spec: [i2: {m1}]
else
  i32.const 1024
  local.get 1
  i32.store
  ;; store spec: [i2: {m2}]
end
;; store spec: [i2: {m1,m2}]
```

This is maybe what we need to do, also for all "spec" domains (vstack, locals, globals, memory).
But then they may require a fixpoint computation, for the spec itself!

### loop
Spec:
```
;; vstack spec: []
;; memory spec: [i3: m0][i5: m1]
;; locals spec: [p0,p1,p2]
block
  loop
    ;; vstack spec: []
    ;; locals spec: [{p0,l0'},{p1,l1'},{p2,l2'}] <- result of fixpoint
    ;; memory spec: [i3: m0][i5: {m1,m2}] <- result of fixpoint
    local.get 2
    ;; vstack spec: [i1]
    ;; constraint: <i1 = p2> JOIN <i1 = l2'>
    i32.eqz
    ;; vstack spec: [i2]
    br_if 1
    ;; vstack spec: []

    ;; copy *l0 into *l1
    local.get 0
    ;; vstack spec: [i3]
    i32.load
    ;; vstack spec: [i4]
    local.get 1
    ;; vstack spec: [i5, i4]
    i32.store
    ;; vstack spec: []
    ;; memory spec: [i3: m0][i5: m2] ;; <- strong update

    ;; increase l0
    local.get 0
    ;; vstack spec: [i6]
    i32.const 1
    ;; vstack spec: [i7, i6]
    i32.add
    ;; vstack spec: [i8]
    local.set 0
    ;; locals spec: [l0',{p1,l1'},{p2,l2'}]

    ;; increase l1
    local.get 1
    ;; vstack spec: [i9]
    i32.const 1
    ;; vstack spec: [i10, i9]
    i32.add
    ;; vstack spec: [i11]
    local.set 0
    ;; vstack spec: []
    ;; locals spec: [l0',l1',{p2,l2'}]

    ;; decrease l2
    local.get l2
    ;; vstack spec: [i12]
    i32.const 1
    ;; vstack spec: [i13,i12]
    i32.sub
    ;; vstack spec: [i14]
    local.set l2
    ;; vstack spec: []
    ;; locals spec: [l0',l1',l2']

    ;; loops
    br 0
```

Analysis
```
;; vstack spec: []
;; memory spec: [i3: m0][i5: m1]
;; locals spec: [p0,p1,p2]
block
  loop
    ;; vstack spec: []
    ;; locals spec: [{p0,l0'},{p1,l1'},{p2,l2'}] <- result of fixpoint
    ;; memory spec: [i3: m0][i5: {m1,m2}] <- result of fixpoint
    local.get 2
    ;; vstack spec: [i1]
    ;; constraint: <i1 \in {p2,l2'}>
    i32.eqz
    ;; vstack spec: [i2]
    br_if 1
    ;; vstack spec: []

    ;; copy *l0 into *l1
    local.get 0
    ;; vstack spec: [i3]
    ;; constraint: <i3 \in {p0, l0'}}
    i32.load
    ;; vstack spec: [i4]
    ;; constraint: <i4 = m0>
    local.get 1
    ;; vstack spec: [i5, i4]
    ;; constraint: <i5 \in {p1, l1'}>
    i32.store
    ;; vstack spec: []
    ;; memory spec: [i3: m0][i5: m2] ;; <- strong update
    ;; constraint: <m2 = i4>

    ;; increase l0
    local.get 0
    ;; vstack spec: [i6]
    i32.const 1
    ;; vstack spec: [i7, i6]
    i32.add
    ;; vstack spec: [i8]
    ;; constraint: none on i8
    local.set 0
    ;; locals spec: [l0',{p1,l1'},{p2,l2'}]
    ;; constraint: <l0' = i8>

    ;; increase l1
    local.get 1
    ;; vstack spec: [i9]
    i32.const 1
    ;; vstack spec: [i10, i9]
    i32.add
    ;; vstack spec: [i11]
    local.set 0
    ;; vstack spec: []
    ;; locals spec: [l0',l1',{p2,l2'}]

    ;; decrease l2
    local.get l2
    ;; vstack spec: [i12]
    ;; constraint: <i12 = p2> JOIN <i12 = l2>
    i32.const 1
    ;; constraint: <i13 = 1>
    ;; vstack spec: [i13,i12]
    i32.sub
    ;; constraint: i14 = i12-i13
    ;; vstack spec: [i14]
    local.set l2
    ;; constraint: <l2' = i14>
    ;; vstack spec: []
    ;; locals spec: [l0',l1',l2']

    ;; loops
    br 0
```

What would be the ideal summary?

`[i3: m0][i5: m1] --> [i3: m0][i5: m2]`
where `m2 = m0`
i3: <i3 \in {p0, l0'}>
l0' = top, but we need to know that it is bounded by p0+p2
this should be achievable with a good domain, but unfortunetaly maybe not simply with polyhedras, or we need to remember the conditions (in this case, that i2 is the result of an eqz test, involving p0/l2').

Again, this is something that could be preprocessed!
We can go backwards to store conditions

```
    ;; cvar i1 depends on {p2,l2'}
    local.get 2
    ;; cvar constraint: ({p2,l2'} == 0) =? 0
    ;; vstack spec: [i1]
    ;; constraint: <i1 \in {p2,l2'}>
    ;; cvar i2 depends on i1
    i32.eqz
    ;; vstack spec: [i2]
    ;; cvar constraint: (i1 == 0) =? 0
    br_if 1
    ;; vstack spec: []
    ;; cvar: [i2]
    ;; cvar constraint: i2 = 0
```

Hence, on the loop body we can restrict p2, l2' to be >= 0
But that's not enough

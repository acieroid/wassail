Static Analysis Of Binary Code With Memory Indirections Using Polyhedra
-------------------------------------------------------------------------------------
VMCAI 2019 best paper is particularly relevant for us!

Abstract domain
-----------------
The abstract domain is very similar: a polyhedra and a model of the runtime structures, containing variables of the polyhedra.
The domain is a triple:
  - P is the polyhedra constraints
  - R (R -> V) is the set of registers, mapping each register name to one variable
  - * (V -> V) is the memory, mapping each memory to a variable

Aliasing definitions
-----------------------
Aliasing is of particular importance for memory operations.
Hence, three sort of aliases are defined:
  - cannot alias: if you meet the polyhedra < x = y > with P and it results in bottom, x cannot alias y
  - may alias: if it results in non-bottom, then x *may* alias y
  - must alias: if P is subsumed by < x = y >

Must alias is denoted as a triple equal sign, that we represent as x === y here

In our case, we do this slightly differently by computing x-y and checking whether this is 0

Interpretation algorithm
----------------------------
The interpretation algorithm is relatively standard, applying widening at loop heads.

Anti-aliasing
---------------
To simplify modeling load/store operations, anti-aliasing is applied to every state to ensure a minimal number of possible cases.

Antialias basically merges two pairs of equivalent addresses, using substitution.
To merge x1 with x2:
  1. in the polyhedra P, x1 is substitued by x2, and in the result of this, *(x1) is substitued by *(x2)
  2. in the memory *, it removes the binding x2

An interesting question is why is anti-aliasing needed, and whether it is related to the fact that they introduce/remove variables.
I don't think it is related, and I think it is only needed to simplify transfer for load/store: after all, bindings are only removed from ...

For us, that means the following:
`antialias(constraints, M)` goes over all pairs of equivalent addresses \in \dom(M), and does:
  - Substitute `x2` by `x1` in constraints, then substitute `M(x2) by M(x1)`
  - Remove `M(x2)`

Consider `M[x: a][y: b]`  with constraints: `b >= 5, a <= 0, x = y` (x = x1, y = x2,)
Then antialias substitutes `x` for `y` in the constraints:
  - `b >= 5, a <= 0`
Then, substitutes `M(x)` for `M(y)`, i.e. `a` for `b` in the constraints:
  - `a >= 5, a <= 0`
Finally, remove `M(y)`:
  - `M[x: a]`
  
An interesting question is: *can we ensure alias-free memories without needing to call antialias at every step?*, and how inefficient is it to call it every time.
Transfer function for binops
---------------------------------
Nothing fancy: if the binop is linear, constraints are added to the polyhedra

Transfer for load
---------------------
It is based on the fact that the state is alias-free.
If it is the case, then "if there is a memory address variable equivalent to the load address" (i.e., there is an a === a' where a' is loaded, and a \in *), then retun *(a).
Otherwise, return top

Basically, if we load `a`, we iterate through the store and check (with the constraints) if `a === a'`, when it does we return the value. If nothing has been returned, we return top.
This is already precisely what load8 does.

Transfer for store
----------------------
Also assumes the state is alias-free.

If there is an address variable equivalent to the target, we can replace it (with `Replace`).
Replace adds the constraint `x = R(srcaddr)` and stores `[targetaddr: x]`
In our case: `x = v` and `M[addr: x]`

Otherwise, we create a new mapping (with `Create`)
Create adds the constraints `x = R(targetaddr), y = R(srcaddr)` and stores `[x: y]`
I don't really see the point of introducing new variables rather than using the already existing ones.
It boils down to just adding a new entry, so in our case we can just do `M[addr: x]` where `x=v`, so nothing changes between replace and create.

However, we also apply an extra `May` step in both cases, to account for may-alias addresses.
May just calls `Replace(a)` for all `a` that may alias with `targetaddr`.

Unification
-------------
The store is joined just like in our case: if a location is bound in one store, unbound in the other, it remains unbound.

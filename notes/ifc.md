# Next steps:
  - count how many variables per function
  - upon memory load/store, apply a backwards analysis or slicing?
  - how to model memory in the summary?
    -> "memory region tainting": this region, starting at x and finishing at y is tainted. x and y can be related from the abstract value (e.g., y = x  ui+5)
  - identify what is trusted/untrusted, or how can this be derived from the summaries.
    This could be defined by the user of the analysis, e.g., function 1's first argument is trusted, second argument is not, return value is not.
    But there is still the question of the memory. Maybe need some notion of trusted/untrusted memory segments.
    

# Non-interference
Non-interference is defined for one input to the program.
In our case, we may need a notion of compositional non-interference?
But probably not: one input to a function always yield the same output (even with different confidential data) is a correct definition for us.
# Papers on abstract domains
## Pentagons: A weakly relational abstract domain for the efficient validation of array accesses
Simpler than octagons, and more efficient, basically computing x € [a,b], x < y (a, b are numbers, x, y are variables)

Probably not strong enough for our case: we need to taint memory regions such that mk = [x, x+y], ie., mk >= x and mk <= x+y.
Octagons are even not enough it seems (x + y <= k), and we need polyhedras (sum ai xi <= k, in our case 1.x + 1.y - 1.mk > 0).

The problem is that even with octagons, up to 4 variables is the max you should have (see refs cited here) to scale. This can be solved using the "technique of buckets".
## A Static Analyzer for Large Safety-Critical Software
Packing for octagons: "Our current strategy is to create one pack for each syn-tactic block in the source code and put in the pack all vari-ables that appear in a linear assignment or test within theassociated block, ignoring what happens in sub-blocks of the block". On average, 4 variables per pack
## SubPolyhedra: a family of numerical abstract domainsfor the (more) scalable inference of linear inequalities
Same precision as polyhedra, but more scalable at the cost of less deductive power
# Papers on slicing
## Interprocedural Static Slicing of Binary Executables (SCAM2003)
If the slice for the statements creating observable behavior is free of statements that processconfidential data, the program is noninterferent [7, 21, 137].
7: M. Abadi, A. Banerjee, N. Heintze, and J. G. Riecke. A core calculus of dependency. InPOPL ’99: Proceedings of the 26th ACM SIGPLAN-SIGACT symposium on Principlesof programming languages, pages 147–160, New York, NY, USA, 1999. ACM
21: ]J.-F. Bergeretti and B. A. Carré.  Information-flow and data-flow analysis of while-programs.ACM Trans. Program. Lang. Syst., 7(1):37–61, 1985.
137: G. Snelting. Combining slicing and constraint solving for validation of measurementsoftware. InStatic Analysis, pages 332–348. Springer-Verlag London, UK, September1996

Basically, a Wasm program has mutliple entry points.
Besides these entry points, we can reason interprocedurally as needed.
## Intraprocedural Static Slicing of Binary Executables (Cifuentes & Fraboulet, 1997)
Could be useful to know which instructions are used to compute memory locations.

# Papers on IFC
## Static Analysis for Inference of Explicit Information Flow (Liu and Milanova, PASTE 2008)
Analysis that works both on complete programs and on "components", applied to Java.

Motivation: dynamic techniques incur run-time overhead [8], and type systems require changing the language to add non-trivial type annotations [33]

Analysis is a client of points-to (Andersen) analysis

Good summary of the vocabulary (implicit, explicit, direct, indirect, confidentiality, integrity)

"**Accessible classes and fields**" are what's public and available by the client. Trusted classes are the ones from the components, others are untrusted.
Question: given a sensitive variable, can a client expose it to untrusted code?

Requires points-to information: aliasing needed to handle information flow through object fields, and call graph needed to approximate possible targets at virtual method calls

Computes **summary** flow graphs
## A static analysis for quantifying information flow in a simple imperative language (Clark, Hunt, and Malacaria, J. Comput. Secur. 2007)
Really interesting idea: instead of simply saying whether there may be a flow from trusted to untrusted variables, it **quantifies** the flow, using Shannon's information theory.

## From dynamic to static and back: Riding the roller coaster of information-flow control research (Sabefield and Russo, 2009)


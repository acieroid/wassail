(** Set to true to print all intermediate variables *)
let show_intermediates = ref false

(** Set to true to print an execution trace *)
let print_trace = ref false

(** Set to true if you want to separate the stack from the heap in the linear memory *)
let disjoint_stack = ref false

(** Set to true to ignore modifications made by imported functions *)
let ignore_imports = ref false

(** Set to true to consider dissferent unknown memory offsets as disjoint memory spaces *)
let disjoint_memory_spaces = ref false
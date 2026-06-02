(* From binaryen, wasm-limits.h: "wasm VMs on the web have decided to impose some limits on what they accept (see e.g. https://github.com/v8/v8/blob/main/src/wasm/wasm-limits.h)." *)

(* TODO: enforce these limitations *)
let max_data_segments = 100_000
let max_table_size = 10_000_000
let max_function_body_size = 128 * 1024
let max_function_locals = 50_000
let max_function_params = 1000

#define WASM_EXPORT __attribute__((visibility("default")))

/* To use from node:
> var fs = require('fs')
> var bytes = fs.readFileSync('console.wasm')
> var importObject = { imports: { print_string: arg => console.log(arg) } };
> var importObject = { 
  imports: { print_string: arg => console.log(arg) },
   module: {},
   env: {
     memory: new WebAssembly.Memory({ initial: 256 }),
     table: new WebAssembly.Table({ initial: 0, element: 'anyfunc' })
   }
  };

 */

typedef signed int i32;

void print_string(i32 *str);

WASM_EXPORT
void propagate_taint(i32 *data) {
  print_string(data);
}

WASM_EXPORT
void ignore_arg(i32 *data) {
  print_string("hello!\n");
}

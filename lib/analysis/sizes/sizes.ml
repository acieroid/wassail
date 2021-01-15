
module T = struct
  (* Size of each section of a WebAssembly module, in bytes *)
  type t = {
    type_section : int;
    import_section : int;
    func_section : int;
    table_section : int;
    memory_section : int;
    global_section : int;
    export_section : int;
    start_section : int;
    elem_section : int;
    code_section : int;
    data_section : int;
  }
end
include T

(* This is mostly the same code as in the spec's interpreter/binary/encode.ml, but used to compute the size of generated sections *)
type stream = {
  buf: Buffer.t;
  patches: (int * char) list ref
}

let stream () : stream = {buf = Buffer.create 8192; patches = ref []}
let pos (s : stream) : int = Buffer.length s.buf
let put (s : stream) (b : char) : unit = Buffer.add_char s.buf b
let put_string (s : stream) (bs : string) : unit = Buffer.add_string s.buf bs
let patch (s : stream) (pos : int) (b : char) : unit = s.patches := (pos, b) :: !(s.patches)

let buf_size (s : stream) : int =
  let bs = Buffer.to_bytes s.buf in
  List.iter (fun (pos, b) -> Bytes.set bs pos b) !(s.patches);
  Bytes.length bs

let sizes (m : Wasm_module.t) : t =
    let s = stream () in

  let module E = struct
    (* Generic values *)

    let u8 i = put s (Char.chr (i land 0xff))
    let u16 i = u8 (i land 0xff); u8 (i lsr 8)
    let u32 i =
      Int32.(u16 (to_int (logand i 0xffffl));
             u16 (to_int (shift_right i 16)))
    let u64 i =
      Int64.(u32 (to_int32 (logand i 0xffffffffL));
             u32 (to_int32 (shift_right i 32)))

    let rec vu64 i =
      let b = Int64.(to_int (logand i 0x7fL)) in
      if 0L <= i && i < 128L then u8 b
      else (u8 (b lor 0x80); vu64 (Int64.shift_right_logical i 7))

    let rec vs64 i =
      let b = Int64.(to_int (logand i 0x7fL)) in
      if -64L <= i && i < 64L then u8 b
      else (u8 (b lor 0x80); vs64 (Int64.shift_right i 7))

    let vu1 i = vu64 Int64.(logand (of_int i) 1L)
    let vu32 i = vu64 Int64.(logand (of_int32 i) 0xffffffffL)
    let vs7 i = vs64 (Int64.of_int i)
    let vs32 i = vs64 (Int64.of_int32 i)
    (* let vs33 i = vs64 (Wasm.I64_convert.extend_i32_s i) *)
    let f32 x = u32 (Wasm.F32.to_bits x)
    let f64 x = u64 (Wasm.F64.to_bits x)

    let len i =
      if Int32.to_int (Int32.of_int i) <> i then
        failwith "cannot encode length with more than 32 bit";
      vu32 (Int32.of_int i)

    let bool b = vu1 (if b then 1 else 0)
    let string bs = len (String.length bs); put_string s bs
    let name n = string n
    let list f xs = List.iter f xs
    let opt f xo = Wasm.Lib.Option.app f xo
    let vec f xs = len (List.length xs); list f xs

    let gap32 () = let p = pos s in u32 0l; u8 0; p
    let patch_gap32 p n =
      assert (n <= 0x0fff_ffff); (* Strings cannot excess 2G anyway *)
      let lsb i = Char.chr (i land 0xff) in
      patch s p (lsb (n lor 0x80));
      patch s (p + 1) (lsb ((n lsr 7) lor 0x80));
      patch s (p + 2) (lsb ((n lsr 14) lor 0x80));
      patch s (p + 3) (lsb ((n lsr 21) lor 0x80));
      patch s (p + 4) (lsb (n lsr 28))

    (* Types *)

    let value_type (t : Type.t) = match t with
      | I32 -> vs7 (-0x01)
      | I64 -> vs7 (-0x02)
      | F32 -> vs7 (-0x03)
      | F64 -> vs7 (-0x04)

    let elem_type = vs7 (-0x10)

    let stack_type = vec value_type
    let func_type ((ins, outs) : Type.t list * Type.t list) =
      vs7 (-0x20); stack_type ins; stack_type outs

    let limits vu (l : Limits.t) =
      let (min, max) = l in
      bool (max <> None); vu min; opt vu max

    let table_type (t : Table.table_type) = elem_type; limits vu32 t

    let memory_type (t : Memory.memory_type) = limits vu32 t

    let mutability = function
      | Global.Immutable -> u8 0
      | Global.Mutable -> u8 1

    let global_type (t : Global.global_type) = value_type t.typ; mutability t.mutability

    (* Expressions *)

    let op n = u8 n
    let end_ () = op 0x0b

    let memop (op : Memoryop.t) =
      vu32 (Int32.of_int op.align); vu32 (Int32.of_int op.offset)

    let var (x : Int32.t) = vu32 x

    let block_type = function
      (* TODO: VarBlockType x -> vs33 x.it *)
      | None -> vs7 (-0x40)
      | Some t -> value_type t

    let rec instr (e : unit Instr.t) = match e with
      | Data i -> begin match i.instr with
          | Nop -> op 0x01
          | Drop -> op 0x1a
          | Select -> op 0x1b

          | LocalGet x -> op 0x20; var x
          | LocalSet x -> op 0x21; var x
          | LocalTee x -> op 0x22; var x
          | GlobalGet x -> op 0x23; var x
          | GlobalSet x -> op 0x24; var x

          | Load ({typ = I32; sz = None; _} as mo) -> op 0x28; memop mo
          | Load ({typ = I64; sz = None; _} as mo) -> op 0x29; memop mo
          | Load ({typ = F32; sz = None; _} as mo) -> op 0x2a; memop mo
          | Load ({typ = F64; sz = None; _} as mo) -> op 0x2b; memop mo
          | Load ({typ = I32; sz = Some (Pack8, SX); _} as mo) -> op 0x2c; memop mo
          | Load ({typ = I32; sz = Some (Pack8, ZX); _} as mo) -> op 0x2d; memop mo
          | Load ({typ = I32; sz = Some (Pack16, SX); _} as mo) -> op 0x2e; memop mo
          | Load ({typ = I32; sz = Some (Pack16, ZX); _} as mo) -> op 0x2f; memop mo
          | Load ({typ = I32; sz = Some (Pack32, _); _}) -> assert false
          | Load ({typ = I64; sz = Some (Pack8, SX); _} as mo) -> op 0x30; memop mo
          | Load ({typ = I64; sz = Some (Pack8, ZX); _} as mo) -> op 0x31; memop mo
          | Load ({typ = I64; sz = Some (Pack16, SX); _} as mo) -> op 0x32; memop mo
          | Load ({typ = I64; sz = Some (Pack16, ZX); _} as mo) -> op 0x33; memop mo
          | Load ({typ = I64; sz = Some (Pack32, SX); _} as mo) -> op 0x34; memop mo
          | Load ({typ = I64; sz = Some (Pack32, ZX); _} as mo) -> op 0x35; memop mo
          | Load ({typ = F32 | F64; sz = Some _; _}) -> assert false

          | Store ({typ = I32; sz = None; _} as mo) -> op 0x36; memop mo
          | Store ({typ = I64; sz = None; _} as mo) -> op 0x37; memop mo
          | Store ({typ = F32; sz = None; _} as mo) -> op 0x38; memop mo
          | Store ({typ = F64; sz = None; _} as mo) -> op 0x39; memop mo
          | Store ({typ = I32; sz = Some (Pack8, _); _} as mo) -> op 0x3a; memop mo
          | Store ({typ = I32; sz = Some (Pack16, _); _} as mo) -> op 0x3b; memop mo
          | Store ({typ = I32; sz = Some (Pack32, _); _}) -> assert false
          | Store ({typ = I64; sz = Some (Pack8, _); _} as mo) -> op 0x3c; memop mo
          | Store ({typ = I64; sz = Some (Pack16, _); _} as mo) -> op 0x3d; memop mo
          | Store ({typ = I64; sz = Some (Pack32, _); _} as mo) -> op 0x3e; memop mo
          | Store ({typ = F32 | F64; sz = Some _; _}) -> assert false

          | MemorySize -> op 0x3f; u8 0x00
          | MemoryGrow -> op 0x40; u8 0x00

          | Const (I32 c) -> op 0x41; vs32 c
          | Const (I64 c) -> op 0x42; vs64 c
          | Const (F32 c) -> op 0x43; f32 c
          | Const (F64 c) -> op 0x44; f64 c

          | Test I32Eqz -> op 0x45
          | Test I64Eqz -> op 0x50

          | Compare ({typ = I32; op = Relop.Eq}) -> op 0x46
          | Compare ({typ = I32; op = Relop.Ne}) -> op 0x47
          | Compare ({typ = I32; op = Relop.LtS}) -> op 0x48
          | Compare ({typ = I32; op = Relop.LtU}) -> op 0x49
          | Compare ({typ = I32; op = Relop.GtS}) -> op 0x4a
          | Compare ({typ = I32; op = Relop.GtU}) -> op 0x4b
          | Compare ({typ = I32; op = Relop.LeS}) -> op 0x4c
          | Compare ({typ = I32; op = Relop.LeU}) -> op 0x4d
          | Compare ({typ = I32; op = Relop.GeS}) -> op 0x4e
          | Compare ({typ = I32; op = Relop.GeU}) -> op 0x4f

          | Compare ({typ = I64; op = Relop.Eq}) -> op 0x51
          | Compare ({typ = I64; op = Relop.Ne}) -> op 0x52
          | Compare ({typ = I64; op = Relop.LtS}) -> op 0x53
          | Compare ({typ = I64; op = Relop.LtU}) -> op 0x54
          | Compare ({typ = I64; op = Relop.GtS}) -> op 0x55
          | Compare ({typ = I64; op = Relop.GtU}) -> op 0x56
          | Compare ({typ = I64; op = Relop.LeS}) -> op 0x57
          | Compare ({typ = I64; op = Relop.LeU}) -> op 0x58
          | Compare ({typ = I64; op = Relop.GeS}) -> op 0x59
          | Compare ({typ = I64; op = Relop.GeU}) -> op 0x5a

          | Compare ({typ = F32; op = Relop.Eq}) -> op 0x5b
          | Compare ({typ = F32; op = Relop.Ne}) -> op 0x5c
          | Compare ({typ = F32; op = Relop.Lt}) -> op 0x5d
          | Compare ({typ = F32; op = Relop.Gt}) -> op 0x5e
          | Compare ({typ = F32; op = Relop.Le}) -> op 0x5f
          | Compare ({typ = F32; op = Relop.Ge}) -> op 0x60

          | Compare ({typ = F64; op = Relop.Eq}) -> op 0x61
          | Compare ({typ = F64; op = Relop.Ne}) -> op 0x62
          | Compare ({typ = F64; op = Relop.Lt}) -> op 0x63
          | Compare ({typ = F64; op = Relop.Gt}) -> op 0x64
          | Compare ({typ = F64; op = Relop.Le}) -> op 0x65
          | Compare ({typ = F64; op = Relop.Ge}) -> op 0x66

          | Unary ({typ = I32; op = Unop.Clz}) -> op 0x67
          | Unary ({typ = I32; op = Unop.Ctz}) -> op 0x68
          | Unary ({typ = I32; op = Unop.Popcnt}) -> op 0x69
          | Unary ({typ = I32; op = Unop.ExtendS Pack8}) -> op 0xc0
          | Unary ({typ = I32; op = Unop.ExtendS Pack16}) -> op 0xc1
          | Unary ({typ = I32; op = Unop.ExtendS Pack32}) -> assert false

          | Unary ({typ = I64; op = Unop.Clz}) -> op 0x79
          | Unary ({typ = I64; op = Unop.Ctz}) -> op 0x7a
          | Unary ({typ = I64; op = Unop.Popcnt}) -> op 0x7b
          | Unary ({typ = I64; op = Unop.ExtendS Pack8}) -> op 0xc2
          | Unary ({typ = I64; op = Unop.ExtendS Pack16}) -> op 0xc3
          | Unary ({typ = I64; op = Unop.ExtendS Pack32}) -> op 0xc4

          | Unary ({typ = F32; op = Unop.Abs}) -> op 0x8b
          | Unary ({typ = F32; op = Unop.Neg}) -> op 0x8c
          | Unary ({typ = F32; op = Unop.Ceil}) -> op 0x8d
          | Unary ({typ = F32; op = Unop.Floor}) -> op 0x8e
          | Unary ({typ = F32; op = Unop.Trunc}) -> op 0x8f
          | Unary ({typ = F32; op = Unop.Nearest}) -> op 0x90
          | Unary ({typ = F32; op = Unop.Sqrt}) -> op 0x91

          | Unary ({typ = F64; op = Unop.Abs}) -> op 0x99
          | Unary ({typ = F64; op = Unop.Neg}) -> op 0x9a
          | Unary ({typ = F64; op = Unop.Ceil}) -> op 0x9b
          | Unary ({typ = F64; op = Unop.Floor}) -> op 0x9c
          | Unary ({typ = F64; op = Unop.Trunc}) -> op 0x9d
          | Unary ({typ = F64; op = Unop.Nearest}) -> op 0x9e
          | Unary ({typ = F64; op = Unop.Sqrt}) -> op 0x9f

          | Binary ({typ = I32; op = Binop.Add}) -> op 0x6a
          | Binary ({typ = I32; op = Binop.Sub}) -> op 0x6b
          | Binary ({typ = I32; op = Binop.Mul}) -> op 0x6c
          | Binary ({typ = I32; op = Binop.DivS}) -> op 0x6d
          | Binary ({typ = I32; op = Binop.DivU}) -> op 0x6e
          | Binary ({typ = I32; op = Binop.RemS}) -> op 0x6f
          | Binary ({typ = I32; op = Binop.RemU}) -> op 0x70
          | Binary ({typ = I32; op = Binop.And}) -> op 0x71
          | Binary ({typ = I32; op = Binop.Or}) -> op 0x72
          | Binary ({typ = I32; op = Binop.Xor}) -> op 0x73
          | Binary ({typ = I32; op = Binop.Shl}) -> op 0x74
          | Binary ({typ = I32; op = Binop.ShrS}) -> op 0x75
          | Binary ({typ = I32; op = Binop.ShrU}) -> op 0x76
          | Binary ({typ = I32; op = Binop.Rotl}) -> op 0x77
          | Binary ({typ = I32; op = Binop.Rotr}) -> op 0x78
          | Binary ({typ = I64; op = Binop.Add}) -> op 0x7c
          | Binary ({typ = I64; op = Binop.Sub}) -> op 0x7d
          | Binary ({typ = I64; op = Binop.Mul}) -> op 0x7e
          | Binary ({typ = I64; op = Binop.DivS}) -> op 0x7f
          | Binary ({typ = I64; op = Binop.DivU}) -> op 0x80
          | Binary ({typ = I64; op = Binop.RemS}) -> op 0x81
          | Binary ({typ = I64; op = Binop.RemU}) -> op 0x82
          | Binary ({typ = I64; op = Binop.And}) -> op 0x83
          | Binary ({typ = I64; op = Binop.Or}) -> op 0x84
          | Binary ({typ = I64; op = Binop.Xor}) -> op 0x85
          | Binary ({typ = I64; op = Binop.Shl}) -> op 0x86
          | Binary ({typ = I64; op = Binop.ShrS}) -> op 0x87
          | Binary ({typ = I64; op = Binop.ShrU}) -> op 0x88
          | Binary ({typ = I64; op = Binop.Rotl}) -> op 0x89
          | Binary ({typ = I64; op = Binop.Rotr}) -> op 0x8a
          | Binary ({typ = F32; op = Binop.Add}) -> op 0x92
          | Binary ({typ = F32; op = Binop.Sub}) -> op 0x93
          | Binary ({typ = F32; op = Binop.Mul}) -> op 0x94
          | Binary ({typ = F32; op = Binop.Div}) -> op 0x95
          | Binary ({typ = F32; op = Binop.Min}) -> op 0x96
          | Binary ({typ = F32; op = Binop.Max}) -> op 0x97
          | Binary ({typ = F32; op = Binop.CopySign}) -> op 0x98
          | Binary ({typ = F64; op = Binop.Add}) -> op 0xa0
          | Binary ({typ = F64; op = Binop.Sub}) -> op 0xa1
          | Binary ({typ = F64; op = Binop.Mul}) -> op 0xa2
          | Binary ({typ = F64; op = Binop.Div}) -> op 0xa3
          | Binary ({typ = F64; op = Binop.Min}) -> op 0xa4
          | Binary ({typ = F64; op = Binop.Max}) -> op 0xa5
          | Binary ({typ = F64; op = Binop.CopySign}) -> op 0xa6

          | Convert ({typ = I32; op = Convertop.ExtendSI32}) -> assert false
          | Convert ({typ = I32; op = Convertop.ExtendUI32}) -> assert false
          | Convert ({typ = I32; op = Convertop.WrapI64}) -> op 0xa7
          | Convert ({typ = I32; op = Convertop.TruncSF32}) -> op 0xa8
          | Convert ({typ = I32; op = Convertop.TruncUF32}) -> op 0xa9
          | Convert ({typ = I32; op = Convertop.TruncSF64}) -> op 0xaa
          | Convert ({typ = I32; op = Convertop.TruncUF64}) -> op 0xab
          | Convert ({typ = I32; op = Convertop.TruncSatSF32}) -> op 0xfc; op 0x00
          | Convert ({typ = I32; op = Convertop.TruncSatUF32}) -> op 0xfc; op 0x01
          | Convert ({typ = I32; op = Convertop.TruncSatSF64}) -> op 0xfc; op 0x02
          | Convert ({typ = I32; op = Convertop.TruncSatUF64}) -> op 0xfc; op 0x03
          | Convert ({typ = I32; op = Convertop.ReinterpretFloat}) -> op 0xbc
          | Convert ({typ = I64; op = Convertop.ExtendSI32}) -> op 0xac
          | Convert ({typ = I64; op = Convertop.ExtendUI32}) -> op 0xad
          | Convert ({typ = I64; op = Convertop.WrapI64}) -> assert false
          | Convert ({typ = I64; op = Convertop.TruncSF32}) -> op 0xae
          | Convert ({typ = I64; op = Convertop.TruncUF32}) -> op 0xaf
          | Convert ({typ = I64; op = Convertop.TruncSF64}) -> op 0xb0
          | Convert ({typ = I64; op = Convertop.TruncUF64}) -> op 0xb1
          | Convert ({typ = I64; op = Convertop.TruncSatSF32}) -> op 0xfc; op 0x04
          | Convert ({typ = I64; op = Convertop.TruncSatUF32}) -> op 0xfc; op 0x05
          | Convert ({typ = I64; op = Convertop.TruncSatSF64}) -> op 0xfc; op 0x06
          | Convert ({typ = I64; op = Convertop.TruncSatUF64}) -> op 0xfc; op 0x07
          | Convert ({typ = I64; op = Convertop.ReinterpretFloat}) -> op 0xbd
          | Convert ({typ = F32; op = Convertop.ConvertSI32}) -> op 0xb2
          | Convert ({typ = F32; op = Convertop.ConvertUI32}) -> op 0xb3
          | Convert ({typ = F32; op = Convertop.ConvertSI64}) -> op 0xb4
          | Convert ({typ = F32; op = Convertop.ConvertUI64}) -> op 0xb5
          | Convert ({typ = F32; op = Convertop.PromoteF32}) -> assert false
          | Convert ({typ = F32; op = Convertop.DemoteF64}) -> op 0xb6
          | Convert ({typ = F32; op = Convertop.ReinterpretInt}) -> op 0xbe
          | Convert ({typ = F64; op = Convertop.ConvertSI32}) -> op 0xb7
          | Convert ({typ = F64; op = Convertop.ConvertUI32}) -> op 0xb8
          | Convert ({typ = F64; op = Convertop.ConvertSI64}) -> op 0xb9
          | Convert ({typ = F64; op = Convertop.ConvertUI64}) -> op 0xba
          | Convert ({typ = F64; op = Convertop.PromoteF32}) -> op 0xbb
          | Convert ({typ = F64; op = Convertop.DemoteF64}) -> assert false
          | Convert ({typ = F64; op = Convertop.ReinterpretInt}) -> op 0xbf
          | _ -> assert false
        end
      | Control c -> begin match c.instr with
          | Unreachable -> op 0x00

          | Block (bt, _arity, es) -> op 0x02; block_type bt; list instr es; end_ ()
          | Loop (bt, _arity, es) -> op 0x03; block_type bt; list instr es; end_ ()
          | If (bt, _arity, es1, es2) ->
            op 0x04; block_type bt; list instr es1;
            if es2 <> [] then op 0x05;
            list instr es2; end_ ()

          | Br x -> op 0x0c; var x
          | BrIf x -> op 0x0d; var x
          | BrTable (xs, x) -> op 0x0e; vec var xs; var x
          | Return -> op 0x0f
          | Call (_, x) -> op 0x10; var x
          | CallIndirect (_, x) -> op 0x11; var x; u8 0x00
          | Merge -> ()
        end


    let const (c : unit Instr.t list) =
      list instr c; end_ ()

    (* Sections *)

    let section (id : int) (f : 'a -> unit) (x : 'a) (needed : bool) =
      if needed then begin
        u8 id;
        let g = gap32 () in
        let p = pos s in
        f x;
        patch_gap32 g (pos s - p)
      end

    (* Type section *)
    let type_ (t : Type.t list * Type.t list) = func_type t

    let type_section (ts : (Type.t list * Type.t list) list) =
       section 1 (vec type_) ts (ts <> [])

    (* Import section *)
    let import_desc (d : Import.desc) =
      match d with
      | FuncImport x -> u8 0x00; var x
      | TableImport t -> u8 0x01; table_type t
      | MemoryImport t -> u8 0x02; memory_type t
      | GlobalImport t -> u8 0x03; global_type t

    let import (im : Import.t) =
      name im.module_name; name im.item_name; import_desc im.idesc

    let import_section ims =
       section 2 (vec import) ims (ims <> [])

    (* Function section *)
    let func (f : Func_inst.t) = var f.type_idx

     let func_section (fs : Func_inst.t list) =
       section 3 (vec func) fs (fs <> [])

    (* Table section *)
     let table (tab : Table.t) =
      table_type tab.ttype

    let table_section tabs =
       section 4 (vec table) tabs (tabs <> [])

    (* Memory section *)
    let memory (mem : Memory.t) =
      memory_type mem.mtype

    let memory_section mems =
       section 5 (vec memory) mems (mems <> [])

    (* Global section *)
    let global (g : Global.t) =
      global_type g.gtype; const g.value

    let global_section gs =
      section 6 (vec global) gs (gs <> [])

    (* Export section *)
    let export_desc (d : Export.desc) =
      match d with
      | FuncExport x -> u8 0; var x
      | TableExport x -> u8 1; var x
      | MemoryExport x -> u8 2; var x
      | GlobalExport x -> u8 3; var x

    let export (ex : Export.t) =
      name ex.name; export_desc ex.edesc

    let export_section exs =
      section 7 (vec export) exs (exs <> [])

    (* Start section *)
    let start_section xo =
      section 8 (opt var) xo (xo <> None)

    (* Code section *)
    let compress ts =
      let combine t = function
        | (t', n) :: ts when t = t' -> (t, n + 1) :: ts
        | ts -> (t, 1) :: ts
      in List.fold_right combine ts []

    let local (t, n) = len n; value_type t

    let code (f : Func_inst.t) =
      let locals = f.code.locals in
      let body = f.code.body in
      let g = gap32 () in
      let p = pos s in
      vec local (compress locals);
      list instr body;
      end_ ();
      patch_gap32 g (pos s - p)

    let code_section (fs : Func_inst.t list) =
      section 10 (vec code) fs (fs <> [])

    (* Element section *)
    let segment_data dat (seg : Segment.DataSegment.t) =
      let index = seg.index in
      let offset = seg.offset in
      let init = seg.init in
      var index; const offset; dat init

    let segment_elem dat (seg : Segment.ElemSegment.t) = (* TODO: extend to any segment *)
      let index = seg.index in
      let offset = seg.offset in
      let init = seg.init in
      var index; const offset; dat init

    let table_segment seg =
      segment_elem (vec var) seg

    let elem_section elems =
       section 9 (vec table_segment) elems (elems <> [])

    (* Data section *)
    let memory_segment seg =
      segment_data string seg

    let data_section data =
      section 11 (vec memory_segment) data (data <> [])

    (* Module *)

    let size (f : unit -> unit) : int =
      let s0 = buf_size s in
      f ();
      let s1 = buf_size s in
      s1 - s0

    let module_ (m : Wasm_module.t) =
      u32 0x6d736100l;
      u32 Wasm.Encode.version;
      let type_section = size (fun () -> type_section m.types) in
      let import_section = size (fun () -> import_section m.imports) in
      let func_section = size (fun () -> func_section m.funcs) in
      let table_section = size (fun () -> table_section m.tables) in
      let memory_section = size (fun () -> memory_section m.memories) in
      let global_section = size (fun () -> global_section m.globals) in
      let export_section = size (fun () -> export_section m.exports) in
      let start_section = size (fun () -> start_section m.start) in
      let elem_section = size (fun () -> elem_section m.elems) in
      let code_section = size (fun () -> code_section m.funcs) in
      let data_section = size (fun () -> data_section m.data) in
      { type_section; import_section; func_section; table_section; memory_section;
        global_section; export_section; start_section; elem_section; code_section; data_section }
  end in
    E.module_ m

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: emit.mlp 10293 2010-04-22 09:33:18Z xleroy $ *)

(* Emission of ARM assembly code *)

open Location
open Misc
open Cmm
open Arch
open Proc
open Reg
open Mach
open Linearize
open Emitaux

(* Tradeoff between code size and code speed *)

let fastcode_flag = ref true

(* Output a label *)

let label_prefix =
  match Config.system with
    "linux_elf" -> ".L"
  | "bsd_elf" -> ".L"
  | "solaris" -> ".L"
  | "beos" -> ".L"
  | "gnu" -> ".L"
  | _ -> "L"

let emit_label lbl =
  emit_string label_prefix; emit_int lbl

(* Output a symbol *)

let symbol_prefix =
  match Config.system with
    "linux_elf" -> ""
  | "bsd_elf" -> ""
  | "solaris" -> ""
  | "beos" -> ""
  | "gnu" -> ""
  | _ -> "_"

let emit_symbol s =
  emit_string symbol_prefix; Emitaux.emit_symbol '$' s

let global_dir =
  match Config.system with
  | "macosx" -> ".globl"
  | _ -> ".global"

(* Output a pseudo-register *)

let emit_reg r =
  match r.loc with
  | Reg r -> emit_string (register_name r)
  | _ -> fatal_error "Emit_arm.emit_reg"

(* Layout of the stack frame *)

let stack_offset = ref 0

let frame_size () =
  let sz =
    !stack_offset +
    4 * num_stack_slots.(0) +
    (if !contains_calls then 4 else 0)
  in Misc.align sz 8

let slot_offset loc cl =
  match loc with
    Incoming n -> frame_size() + n
  | Local n -> !stack_offset + n * 4
  | Outgoing n -> n

(* Output a stack reference *)

let emit_stack r =
  match r.loc with
  | Stack s ->
      let ofs = slot_offset s (register_class r) in (emit_string "[sp, #"; emit_int ofs; emit_char ']')
  | _ -> fatal_error "Emit_arm.emit_stack"

(* Output an addressing mode *)

let emit_addressing addr r n =
  match addr with
    Iindexed ofs ->
      (emit_char '['; emit_reg r.(n); emit_string ", #"; emit_int ofs; emit_char ']')

(* Record live pointers at call points *)

type frame_descr =
  { fd_lbl: int;                        (* Return address *)
    fd_frame_size: int;                 (* Size of stack frame *)
    fd_live_offset: int list }          (* Offsets/regs of live addresses *)

let frame_descriptors = ref([] : frame_descr list)

let record_frame live =
  let lbl = new_label() in
  let live_offset = ref [] in
  Reg.Set.iter
    (function
        {typ = Addr; loc = Reg r} ->
          live_offset := (r lsl 1) + 1 :: !live_offset
      | {typ = Addr; loc = Stack s} as reg ->
          live_offset := slot_offset s (register_class reg) :: !live_offset
      | _ -> ())
    live;
  frame_descriptors :=
    { fd_lbl = lbl;
      fd_frame_size = frame_size();
      fd_live_offset = !live_offset } :: !frame_descriptors;
  (emit_label lbl; emit_char ':')

let emit_frame fd =
  (emit_string "	.word	"; emit_label fd.fd_lbl; emit_string " + 4\n");
  (emit_string "	.short	"; emit_int fd.fd_frame_size; emit_char '\n');
  (emit_string "	.short	"; emit_int (List.length fd.fd_live_offset); emit_char '\n');
  List.iter
    (fun n ->
      (emit_string "	.short	"; emit_int n; emit_char '\n'))
    fd.fd_live_offset;
  (emit_string "	.align	2\n")

(* Names of various instructions *)

let name_for_comparison = function
    Isigned Ceq -> "eq" | Isigned Cne -> "ne" | Isigned Cle -> "le"
  | Isigned Cge -> "ge" | Isigned Clt -> "lt" | Isigned Cgt -> "gt"
  | Iunsigned Ceq -> "eq" | Iunsigned Cne -> "ne" | Iunsigned Cle -> "ls"
  | Iunsigned Cge -> "cs" | Iunsigned Clt -> "cc" | Iunsigned Cgt -> "hi"

let name_for_float_comparison cmp neg =
  match cmp with
    Ceq -> if neg then "ne" else "eq"
  | Cne -> if neg then "eq" else "ne"
  | Cle -> if neg then "hi" else "ls"
  | Cge -> if neg then "lt" else "ge"
  | Clt -> if neg then "pl" else "mi"
  | Cgt -> if neg then "le" else "gt"

let name_for_int_operation = function
    Iadd -> "add"
  | Isub -> "sub"
  | Imul -> "mul"
  | Iand  -> "and"
  | Ior   -> "orr"
  | Ixor  -> "eor"
  | _ -> assert false

let name_for_shift_operation = function
    Ilsl -> "lsl"
  | Ilsr -> "lsr"
  | Iasr -> "asr"
  | _ -> assert false

let name_for_shift_int_operation = function
    Ishiftadd -> "add"
  | Ishiftsub -> "sub"
  | Ishiftsubrev -> "rsb"

(* Recognize immediate operands *)

(* Immediate operands are 8-bit immediate values, zero-extended, and rotated
   right by 0, 2, 4, ... 30 bits.
   We check only with 8-bit values shifted left 0 to 24 bits. *)

let rec is_immed n shift =
  shift <= 24 &&
  (Nativeint.logand n (Nativeint.shift_left (Nativeint.of_int 0xFF) shift) = n
   || is_immed n (shift + 2))

let is_immediate n = is_immed n 0

(* General functional to decompose a non-immediate integer constant
   into 8-bit chunks shifted left 0 ... 24 bits *)

let decompose_intconst n fn =
  let i = ref n in
  let shift = ref 0 in
  let ninstr = ref 0 in
  while !i <> 0n do
    if Nativeint.to_int (Nativeint.shift_right !i !shift) land 3 = 0 then
      shift := !shift + 2
    else begin
      let mask = Nativeint.shift_left 0xFFn !shift in
      let bits = Nativeint.logand !i mask in
      fn bits;
      shift := !shift + 8;
      i := Nativeint.sub !i bits;
      incr ninstr
    end
  done;
  !ninstr

(* Load an integer constant into a register *)

let emit_intconst r n =
  let nr = Nativeint.lognot n in
  if is_immediate n then begin
    (emit_string "	mov	"; emit_reg r; emit_string ", #"; emit_nativeint n; emit_char '\n'); 1
  end else if is_immediate nr then begin
    (emit_string "	mvn	"; emit_reg r; emit_string ", #"; emit_nativeint nr; emit_char '\n'); 1
  end else begin
    let first = ref true in
    decompose_intconst n
      (fun bits ->
        if !first
        then (emit_string "	mov	"; emit_reg r; emit_string ", #"; emit_nativeint bits; emit_string " @ "; emit_nativeint n; emit_char '\n')
        else (emit_string "	add	"; emit_reg r; emit_string ", "; emit_reg r; emit_string ", #"; emit_nativeint bits; emit_char '\n');
        first := false)
  end

(* Adjust sp (up or down) by the given byte amount *)

let emit_stack_adjustment instr n =
  if n <= 0 then 0 else
    decompose_intconst (Nativeint.of_int n)
      (fun bits ->
        (emit_char '	'; emit_string instr; emit_string "	sp, sp, #"; emit_nativeint bits; emit_char '\n'))

(* Name of current function *)
let function_name = ref ""
(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref 0
(* Table of symbols referenced *)
let symbol_constants = (Hashtbl.create 11 : (string, int) Hashtbl.t)
(* Table of floating-point literals *)
let float_constants = (Hashtbl.create 11 : (string, int) Hashtbl.t)
(* Total space (in word) occupied by pending literals *)
let num_literals = ref 0

(* Label a symbol or float constant *)
let label_constant tbl s size =
  try
    Hashtbl.find tbl s
  with Not_found ->
    let lbl = new_label() in
    Hashtbl.add tbl s lbl;
    num_literals := !num_literals + size;
    lbl

(* Emit all pending constants *)

let emit_constants () =
  Hashtbl.iter
    (fun s lbl ->
      (emit_label lbl; emit_string ":        .word   "; emit_symbol s; emit_char '\n'))
    symbol_constants;
  Hashtbl.iter
    (fun s lbl ->
      (emit_label lbl; emit_string ":        .double "; emit_string s; emit_char '\n'))
    float_constants;
  Hashtbl.clear symbol_constants;
  Hashtbl.clear float_constants;
  num_literals := 0

(* Output the assembly code for an instruction *)

let emit_instr i =
    match i.desc with
      Lend -> 0
    | Lop(Imove | Ispill | Ireload) ->
        let src = i.arg.(0) and dst = i.res.(0) in
        if src.loc = dst.loc then 0 else begin
          match (src, dst) with
            {loc = Reg rs; typ = Int|Addr}, {loc = Reg rd; typ = Int|Addr} ->
              (emit_string "	mov	"; emit_reg dst; emit_string ", "; emit_reg src; emit_char '\n'); 1
          | {loc = Reg rs; typ = Int|Addr}, {loc = Stack sd} ->
              (emit_string "	str	"; emit_reg src; emit_string ", "; emit_stack dst; emit_char '\n'); 1
          | {loc = Stack ss; typ = Int|Addr}, {loc = Reg rd} ->
              (emit_string "	ldr	"; emit_reg dst; emit_string ", "; emit_stack src; emit_char '\n'); 1
          | _ ->
              assert false
        end
    | Lop(Iconst_int n) ->
        emit_intconst i.res.(0) n
    | Lop(Iconst_float s) ->
        let bits = Int64.bits_of_float (float_of_string s) in
        let high_bits = Int64.to_nativeint (Int64.shift_right_logical bits 32)
        and low_bits = Int64.to_nativeint bits in
        if is_immediate low_bits && is_immediate high_bits then begin
          (emit_string "	mov	"; emit_reg i.res.(0); emit_string ", #"; emit_nativeint low_bits; emit_string " @ "; emit_string s; emit_char '\n');
          (emit_string "	mov	"; emit_reg i.res.(1); emit_string ", #"; emit_nativeint high_bits; emit_char '\n');
          2
        end else begin
          let lbl = label_constant float_constants s 2 in
          (emit_string "	ldr	"; emit_reg i.res.(0); emit_string ", "; emit_label lbl; emit_string " @ "; emit_string s; emit_char '\n');
          (emit_string "	ldr	"; emit_reg i.res.(1); emit_string ", "; emit_label lbl; emit_string " + 4\n");
          2
        end
    | Lop(Iconst_symbol s) ->
        let lbl = label_constant symbol_constants s 1 in
        (emit_string "	ldr	"; emit_reg i.res.(0); emit_string ", "; emit_label lbl; emit_string " @ "; emit_symbol s; emit_char '\n'); 1
    | Lop(Icall_ind) ->
        (emit_string "	mov	lr, pc\n");
        (record_frame i.live; emit_string "  bx	"; emit_reg i.arg.(0); emit_char '\n'); 2
    | Lop(Icall_imm s) ->
        (record_frame i.live; emit_string "  bl      "; emit_symbol s; emit_char '\n'); 1
    | Lop(Itailcall_ind) ->
        let n = frame_size() in
        if !contains_calls then
          (emit_string "	ldr	lr, [sp, #"; emit_int (n-4); emit_string "]\n");
        let ninstr = emit_stack_adjustment "add" n in
        (emit_string "	bx	"; emit_reg i.arg.(0); emit_char '\n');
        2 + ninstr
    | Lop(Itailcall_imm s) ->
        if s = !function_name then begin
          (emit_string "	b	"; emit_label !tailrec_entry_point; emit_char '\n'); 1
        end else begin
          let n = frame_size() in
          if !contains_calls then
            (emit_string "	ldr	lr, [sp, #"; emit_int (n-4); emit_string "]\n");
          let ninstr = emit_stack_adjustment "add" n in
          (emit_string "	b	"; emit_symbol s; emit_char '\n');
          2 + ninstr
        end
    | Lop(Iextcall(s, alloc)) ->
        if alloc then begin
          let lbl = label_constant symbol_constants s 1 in
          (emit_string "	ldr	r12, "; emit_label lbl; emit_string " @ "; emit_symbol s; emit_char '\n');
          (record_frame i.live; emit_string "	bl	"; emit_symbol "caml_c_call"; emit_char '\n'); 2
        end else begin
          (emit_string "	bl	"; emit_symbol s; emit_char '\n'); 1
        end
    | Lop(Istackoffset n) ->
        assert (n mod 8 = 0);
        let ninstr =
          if n >= 0
          then emit_stack_adjustment "sub" n
          else emit_stack_adjustment "add" (-n) in
        stack_offset := !stack_offset + n;
        ninstr
    | Lop(Iload((Double | Double_u), addr)) ->
        let addr' = offset_addressing addr 4 in
        if i.res.(0).loc <> i.arg.(0).loc then begin
          (emit_string "	ldr	"; emit_reg i.res.(0); emit_string ", "; emit_addressing addr i.arg 0; emit_char '\n');
          (emit_string "	ldr	"; emit_reg i.res.(1); emit_string ", "; emit_addressing addr' i.arg 0; emit_char '\n')
        end else begin
          (emit_string "	ldr	"; emit_reg i.res.(1); emit_string ", "; emit_addressing addr' i.arg 0; emit_char '\n');
          (emit_string "	ldr	"; emit_reg i.res.(0); emit_string ", "; emit_addressing addr i.arg 0; emit_char '\n')
        end;
        2
    | Lop(Iload(size, addr)) ->
        let r = i.res.(0) in
        let instr =
          match size with
            Byte_unsigned -> "ldrb"
          | Byte_signed -> "ldrsb"
          | Sixteen_unsigned -> "ldrh"
          | Sixteen_signed -> "ldrsh"
          | _ (* 32-bit quantities *) -> "ldr" in
        (emit_char '	'; emit_string	instr; emit_string "     "; emit_reg r; emit_string ", "; emit_addressing addr i.arg 0; emit_char '\n');
        1
    | Lop(Istore((Double | Double_u), addr)) ->
        let addr' = offset_addressing addr 4 in
        (emit_string "	str	"; emit_reg i.arg.(0); emit_string ", "; emit_addressing addr i.arg 2; emit_char '\n');
        (emit_string "	str	"; emit_reg i.arg.(1); emit_string ", "; emit_addressing addr' i.arg 2; emit_char '\n');
        2
    | Lop(Istore(size, addr)) ->
        let r = i.arg.(0) in
        let instr =
          match size with
            Byte_unsigned | Byte_signed -> "strb"
          | Sixteen_unsigned | Sixteen_signed -> "strh"
          | _ (* 32-bit quantities *) -> "str" in
        (emit_char '	'; emit_string	instr; emit_string "     "; emit_reg r; emit_string ", "; emit_addressing addr i.arg 1; emit_char '\n');
        1
    | Lop(Ialloc n) ->
        if !fastcode_flag then begin
          let ni = emit_intconst (phys_reg 8 (*r12*)) (Nativeint.of_int n) in
          (emit_string "	sub	r8, r8, r12\n");
          (emit_string "	cmp	r8, r10\n");
          (record_frame i.live; emit_string "	blcc    "; emit_symbol "caml_call_gc"; emit_char '\n');
          (emit_string "	add	"; emit_reg i.res.(0); emit_string ", r8, #4\n");
          4 + ni
        end else if n = 8 || n = 12 || n = 16 then begin
          (record_frame i.live; emit_string "	bl	"; emit_symbol "caml_alloc"; emit_int ((n-4)/4); emit_char '\n');
          (emit_string "	add	"; emit_reg i.res.(0); emit_string ", r8, #4\n"); 2
        end else begin
          let ni = emit_intconst (phys_reg 8 (*r12*)) (Nativeint.of_int n) in
          (record_frame i.live; emit_string "	bl	"; emit_symbol "caml_allocN"; emit_char '\n');
          (emit_string "	add	"; emit_reg i.res.(0); emit_string ", r8, #4\n");
          2 + ni
        end
    | Lop(Iintop(Ilsl | Ilsr | Iasr as op)) ->
        let shift = name_for_shift_operation op in
        (emit_string "	mov	"; emit_reg i.res.(0); emit_string ", "; emit_reg i.arg.(0); emit_string ", "; emit_string shift; emit_char ' '; emit_reg i.arg.(1); emit_char '\n'); 1
    | Lop(Iintop(Icomp cmp)) ->
        let comp = name_for_comparison cmp in
        (emit_string "	cmp	"; emit_reg i.arg.(0); emit_string ", "; emit_reg i.arg.(1); emit_char '\n');
        (emit_string "	mov	"; emit_reg i.res.(0); emit_string ", #0\n");
        (emit_string "	mov"; emit_string	comp; emit_string "   "; emit_reg i.res.(0); emit_string ", #1\n"); 3
    | Lop(Iintop(Icheckbound)) ->
        (emit_string "	cmp	"; emit_reg i.arg.(0); emit_string ", "; emit_reg i.arg.(1); emit_char '\n');
        (emit_string "	blls	"; emit_symbol "caml_ml_array_bound_error"; emit_char '\n'); 2
    | Lop(Iintop op) ->
        let instr = name_for_int_operation op in
        (emit_char '	'; emit_string	instr; emit_string "     "; emit_reg i.res.(0); emit_string ", "; emit_reg i.arg.(0); emit_string ", "; emit_reg i.arg.(1); emit_char '\n'); 1
    | Lop(Iintop_imm(Idiv, n)) -> (* n is a power of 2 *)
        let l = Misc.log2 n in
        let r = i.res.(0) in
        (emit_string "	movs	"; emit_reg r; emit_string ", "; emit_reg i.arg.(0); emit_char '\n');
        if n <= 256 then
          (emit_string "	addlt	"; emit_reg r; emit_string ", "; emit_reg r; emit_string ", #"; emit_int (n-1); emit_char '\n')
        else begin
          (emit_string "	addlt	"; emit_reg r; emit_string ", "; emit_reg r; emit_string ", #"; emit_int n; emit_char '\n');
          (emit_string "	sublt	"; emit_reg r; emit_string ", "; emit_reg r; emit_string ", #1\n")
        end;
        (emit_string "	mov	"; emit_reg r; emit_string ", "; emit_reg r; emit_string ", asr #"; emit_int l; emit_char '\n'); 4
    | Lop(Iintop_imm(Imod, n)) -> (* n is a power of 2 *)
        let l = Misc.log2 n in
        let a = i.arg.(0) in
        let r = i.res.(0) in
        let lbl = new_label() in
        (emit_string "	cmp	"; emit_reg a; emit_string ", #0\n");
        (emit_string "	mov	"; emit_reg r; emit_string ", "; emit_reg a; emit_string ", lsl #"; emit_int (32-l); emit_char '\n');
        (emit_string "	mov	"; emit_reg r; emit_string ", "; emit_reg r; emit_string ", lsr #"; emit_int (32-l); emit_char '\n');
        (emit_string "	bpl	"; emit_label lbl; emit_char '\n');
        (emit_string "	cmp	"; emit_reg r; emit_string ", #0\n");
        (emit_string "	subne	"; emit_reg r; emit_string ", "; emit_reg r; emit_string ", #"; emit_int n; emit_char '\n');
        (emit_label lbl; emit_string ":\n"); 6
    | Lop(Iintop_imm((Ilsl | Ilsr | Iasr as op), n)) ->
        let shift = name_for_shift_operation op in
        (emit_string "	mov	"; emit_reg i.res.(0); emit_string ", "; emit_reg i.arg.(0); emit_string ", "; emit_string shift; emit_string " #"; emit_int n; emit_char '\n'); 1
    | Lop(Iintop_imm(Icomp cmp, n)) ->
        let comp = name_for_comparison cmp in
        (emit_string "	cmp	"; emit_reg i.arg.(0); emit_string ", #"; emit_int n; emit_char '\n');
        (emit_string "	mov	"; emit_reg i.res.(0); emit_string ", #0\n");
        (emit_string "	mov"; emit_string	comp; emit_string "   "; emit_reg i.res.(0); emit_string ", #1\n"); 3
    | Lop(Iintop_imm(Icheckbound, n)) ->
        (emit_string "	cmp	"; emit_reg i.arg.(0); emit_string ", #"; emit_int n; emit_char '\n');
        (emit_string "	blls	"; emit_symbol "caml_ml_array_bound_error"; emit_char '\n'); 2
    | Lop(Iintop_imm(op, n)) ->
        let instr = name_for_int_operation op in
        (emit_char '	'; emit_string	instr; emit_string "     "; emit_reg i.res.(0); emit_string ", "; emit_reg i.arg.(0); emit_string ", #"; emit_int n; emit_char '\n'); 1
    | Lop(Inegf) -> (* argument and result in (r0, r1) *)
        (emit_string "	eor     r1, r1, #0x80000000\n"); 1
    | Lop(Iabsf) -> (* argument and result in (r0, r1) *)
        (emit_string "	bic     r1, r1, #0x80000000\n"); 1
    | Lop(Ifloatofint | Iintoffloat | Iaddf | Isubf | Imulf | Idivf) ->
        assert false
    | Lop(Ispecific(Ishiftarith(op, shift))) ->
        let instr = name_for_shift_int_operation op in
        (emit_char '	'; emit_string	instr; emit_string "     "; emit_reg i.res.(0); emit_string ", "; emit_reg i.arg.(0); emit_string ", "; emit_reg i.arg.(1));
        if shift >= 0
        then (emit_string ", lsl #"; emit_int shift; emit_char '\n')
        else (emit_string ", asr #"; emit_int (-shift); emit_char '\n');
        1
    | Lop(Ispecific(Ishiftcheckbound shift)) ->
        (emit_string "	cmp	"; emit_reg i.arg.(1); emit_string ", "; emit_reg i.arg.(0); emit_string ", lsr #"; emit_int shift; emit_char '\n');
        (emit_string "	blcs	"; emit_symbol "caml_ml_array_bound_error"; emit_char '\n'); 2
    | Lop(Ispecific(Irevsubimm n)) ->
        (emit_string "	rsb	"; emit_reg i.res.(0); emit_string ", "; emit_reg i.arg.(0); emit_string ", #"; emit_int n; emit_char '\n'); 1
    | Lreloadretaddr ->
        let n = frame_size() in
        (emit_string "	ldr	lr, [sp, #"; emit_int(n-4); emit_string "]\n"); 1
    | Lreturn ->
        let ninstr = emit_stack_adjustment "add" (frame_size()) in
        (emit_string "	bx	lr\n");
        ninstr + 1
    | Llabel lbl ->
        (emit_label lbl; emit_string ":\n"); 0
    | Lbranch lbl ->
        (emit_string "	b	"; emit_label lbl; emit_char '\n'); 1
    | Lcondbranch(tst, lbl) ->
        begin match tst with
          Itruetest ->
            (emit_string "	cmp	"; emit_reg i.arg.(0); emit_string ", #0\n");
            (emit_string "	bne	"; emit_label lbl; emit_char '\n')
        | Ifalsetest ->
            (emit_string "	cmp	"; emit_reg i.arg.(0); emit_string ", #0\n");
            (emit_string "	beq	"; emit_label lbl; emit_char '\n')
        | Iinttest cmp ->
            (emit_string "	cmp	"; emit_reg i.arg.(0); emit_string ", "; emit_reg i.arg.(1); emit_char '\n');
            let comp = name_for_comparison cmp in
            (emit_string "	b"; emit_string comp; emit_char '	'; emit_label lbl; emit_char '\n')
        | Iinttest_imm(cmp, n) ->
            (emit_string "	cmp	"; emit_reg i.arg.(0); emit_string ", #"; emit_int n; emit_char '\n');
            let comp = name_for_comparison cmp in
            (emit_string "	b"; emit_string comp; emit_char '	'; emit_label lbl; emit_char '\n')
        | Ifloattest(cmp, neg) ->
            assert false
        | Ioddtest ->
            (emit_string "	tst	"; emit_reg i.arg.(0); emit_string ", #1\n");
            (emit_string "	bne	"; emit_label lbl; emit_char '\n')
        | Ieventest ->
            (emit_string "	tst	"; emit_reg i.arg.(0); emit_string ", #1\n");
            (emit_string "	beq	"; emit_label lbl; emit_char '\n')
        end;
        2
  | Lcondbranch3(lbl0, lbl1, lbl2) ->
        (emit_string "	cmp	"; emit_reg i.arg.(0); emit_string ", #1\n");
        begin match lbl0 with
          None -> ()
        | Some lbl -> (emit_string "	blt	"; emit_label lbl; emit_char '\n')
        end;
        begin match lbl1 with
          None -> ()
        | Some lbl -> (emit_string "	beq	"; emit_label lbl; emit_char '\n')
        end;
        begin match lbl2 with
          None -> ()
        | Some lbl -> (emit_string "	bgt	"; emit_label lbl; emit_char '\n')
        end;
        4
  | Lswitch jumptbl ->
        (emit_string "	ldr	pc, [pc, "; emit_reg i.arg.(0); emit_string ", lsl #2]\n");
        (emit_string "	mov	r0, r0\n");      (* nop *)
        for i = 0 to Array.length jumptbl - 1 do
          (emit_string "	.word	"; emit_label jumptbl.(i); emit_char '\n')
        done;
        2 + Array.length jumptbl
    | Lsetuptrap lbl ->
        (emit_string "	bl	"; emit_label lbl; emit_char '\n'); 1
    | Lpushtrap ->
        stack_offset := !stack_offset + 8;
        (emit_string "	stmfd	sp!, {r11, lr}\n");
        (emit_string "	mov	r11, sp\n"); 2
    | Lpoptrap ->
        (emit_string "	ldmfd	sp!, {r11, lr}\n");
        stack_offset := !stack_offset - 8; 1
    | Lraise ->
        (emit_string "	mov	sp, r11\n");
        (emit_string "	ldmfd	sp!, {r11, pc}\n"); 2

(* Emission of an instruction sequence *)

let no_fallthrough = function
    Lop(Itailcall_ind | Itailcall_imm _) -> true
  | Lreturn -> true
  | Lbranch _ -> true
  | Lswitch _ -> true
  | Lraise -> true
  | _ -> false

let rec emit_all ninstr i =
  if i.desc = Lend then () else begin
    let n = emit_instr i in
    let ninstr' = ninstr + n in
    let limit = 511 - !num_literals in
    if ninstr' >= limit - 64 && no_fallthrough i.desc then begin
      emit_constants();
      emit_all 0 i.next
    end else
    if ninstr' >= limit then begin
      let lbl = new_label() in
      (emit_string "	b	"; emit_label lbl; emit_char '\n');
      emit_constants();
      (emit_label lbl; emit_string ":\n");
      emit_all 0 i.next
    end else
      emit_all ninstr' i.next
  end

(* Emission of a function declaration *)

let fundecl fundecl =
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point := new_label();
  stack_offset := 0;
  Hashtbl.clear symbol_constants;
  Hashtbl.clear float_constants;
  (emit_string "	.text\n");
  (emit_string "	.align	2\n");
  (emit_char '	'; emit_string global_dir; emit_char '	'; emit_symbol fundecl.fun_name; emit_char '\n');
  (emit_symbol fundecl.fun_name; emit_string ":\n");
  let n = frame_size() in
  ignore(emit_stack_adjustment "sub" n);
  if !contains_calls then
    (emit_string "	str	lr, [sp, #"; emit_int(n - 4); emit_string "]\n");
  (emit_label !tailrec_entry_point; emit_string ":\n");
  emit_all 0 fundecl.fun_body;
  emit_constants()

(* Emission of data *)

let emit_item = function
    Cglobal_symbol s ->
      (emit_char '	'; emit_string global_dir; emit_char '	'; emit_symbol s; emit_char '\n');
  | Cdefine_symbol s ->
      (emit_symbol s; emit_string ":\n")
  | Cdefine_label lbl ->
      (emit_label (100000 + lbl); emit_string ":\n")
  | Cint8 n ->
      (emit_string "	.byte	"; emit_int n; emit_char '\n')
  | Cint16 n ->
      (emit_string "	.short	"; emit_int n; emit_char '\n')
  | Cint32 n ->
      (emit_string "	.word	"; emit_nativeint	n; emit_char '\n')
  | Cint n ->
      (emit_string "	.word	"; emit_nativeint n; emit_char '\n')
  | Csingle f ->
      emit_float32_directive ".long" f
  | Cdouble f ->
      emit_float64_split_directive ".long" f
  | Csymbol_address s ->
      (emit_string "	.word	"; emit_symbol s; emit_char '\n')
  | Clabel_address lbl ->
      (emit_string "	.word	"; emit_label (100000 + lbl); emit_char '\n')
  | Cstring s ->
      emit_string_directive "	.ascii  " s
  | Cskip n ->
      if n > 0 then (emit_string "	.space	"; emit_int n; emit_char '\n')
  | Calign n ->
      (emit_string "	.align	"; emit_int(Misc.log2 n); emit_char '\n')

let data l =
  (emit_string "	.data\n");
  List.iter emit_item l

(* Beginning / end of an assembly file *)

let begin_assembly() =
  (emit_string "#define trap_ptr r11\n");
  (emit_string "#define alloc_ptr r8\n");
  (emit_string "#define alloc_limit r10\n");
  let lbl_begin = Compilenv.make_symbol (Some "data_begin") in
  (emit_string "	.data\n");
  (emit_char '	'; emit_string global_dir; emit_char '	'; emit_symbol lbl_begin; emit_char '\n');
  (emit_symbol lbl_begin; emit_string ":\n");
  let lbl_begin = Compilenv.make_symbol (Some "code_begin") in
  (emit_string "	.text\n");
  (emit_char '	'; emit_string global_dir; emit_char '	'; emit_symbol lbl_begin; emit_char '\n');
  (emit_symbol lbl_begin; emit_string ":\n")

let end_assembly () =
  let lbl_end = Compilenv.make_symbol (Some "code_end") in
  (emit_string "	.text\n");
  (emit_char '	'; emit_string global_dir; emit_char '	'; emit_symbol lbl_end; emit_char '\n');
  (emit_symbol lbl_end; emit_string ":\n");
  let lbl_end = Compilenv.make_symbol (Some "data_end") in
  (emit_string "	.data\n");
  (emit_char '	'; emit_string global_dir; emit_char '	'; emit_symbol lbl_end; emit_char '\n');
  (emit_symbol lbl_end; emit_string ":\n");
  (emit_string "	.word	0\n");
  let lbl = Compilenv.make_symbol (Some "frametable") in
  (emit_string "	.data\n");
  (emit_char '	'; emit_string global_dir; emit_char '	'; emit_symbol lbl; emit_char '\n');
  (emit_symbol lbl; emit_string ":\n");
  (emit_string "	.word	"; emit_int (List.length !frame_descriptors); emit_char '\n');
  List.iter emit_frame !frame_descriptors;
  frame_descriptors := []

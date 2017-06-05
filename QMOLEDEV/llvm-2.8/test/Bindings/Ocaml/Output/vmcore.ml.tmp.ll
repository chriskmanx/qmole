; ModuleID = '<stdin>'
target datalayout = "bogus"
target triple = "i686-apple-darwin8"

%0 = type { i16, i16, i32, i32 }
%1 = type <{ i1, i8, i64, double }>
%array_type = type [7 x i8]
%double_type = type double
%float_type = type float
%function_type = type i32 (i1, double)
%i1_type = type i1
%i32_type = type i32
%i42_type = type i42
%opaque_type = type opaque
%pointer_type = type float*
%qualified_pointer_type = type i8 addrspace(3)*
%recursive_type = type %recursive_type*
%type_name = type opaque
%var_arg_type = type void (i32, ...)
%vector_type = type <4 x i16>
%void_type = type void

@const_int = global i32 -1
@const_sext_int = global i64 -1
@const_zext_int64 = global i64 4294967295
@const_int_string = global i32 -1
@const_string = global [11 x i8] c"cruel\00world"
@const_stringz = global [9 x i8] c"hi\00again\00"
@const_single = global float 0.000000e+00
@const_double = global double 0x9E83E42640092ACD
@const_double_string = global double 1.250000e+00
@const_array = global [2 x i32] [i32 3, i32 4]
@const_vector = global <8 x i16> <i16 1, i16 2, i16 1, i16 2, i16 1, i16 2, i16 1, i16 2>
@const_structure = global %0 { i16 1, i16 2, i32 3, i32 4 }
@const_null = global %1 zeroinitializer
@const_all_ones = global i64 -1
@const_pointer_null = global i64* null
@const_undef = global i1 undef
@FoldBomb = global i8 0
@const_neg = global i64 sub (i64 0, i64 ptrtoint (i8* @FoldBomb to i64))
@const_nsw_neg = global i64 sub nsw (i64 0, i64 ptrtoint (i8* @FoldBomb to i64))
@const_nuw_neg = global i64 sub nuw (i64 0, i64 ptrtoint (i8* @FoldBomb to i64))
@const_fneg = global double fsub (double -0.000000e+00, double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double))
@const_not = global i64 xor (i64 ptrtoint (i8* @FoldBomb to i64), i64 -1)
@const_add = global i64 add (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_nsw_add = global i64 add nsw (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_nuw_add = global i64 add nuw (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_fadd = global double fadd (double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double), double 5.000000e+00)
@const_sub = global i64 sub (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_nsw_sub = global i64 sub nsw (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_nuw_sub = global i64 sub nuw (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_fsub = global double fsub (double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double), double 5.000000e+00)
@const_mul = global i64 mul (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_nsw_mul = global i64 mul nsw (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_nuw_mul = global i64 mul nuw (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_fmul = global double fmul (double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double), double 5.000000e+00)
@const_udiv = global i64 udiv (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_sdiv = global i64 sdiv (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_exact_sdiv = global i64 sdiv exact (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_fdiv = global double fdiv (double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double), double 5.000000e+00)
@const_urem = global i64 urem (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_srem = global i64 srem (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_frem = global double frem (double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double), double 5.000000e+00)
@const_and = global i64 and (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_or = global i64 or (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_xor = global i64 xor (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_icmp = global i1 icmp sle (i64 ptrtoint (i8* @FoldBomb to i64), i64 5)
@const_fcmp = global i1 fcmp ole (double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double), double 5.000000e+00)
@const_trunc = global i8 trunc (i64 add (i64 ptrtoint (i8* @FoldBomb to i64), i64 5) to i8)
@const_sext = global i128 sext (i64 ptrtoint (i8* @FoldBomb to i64) to i128)
@const_zext = global i128 zext (i64 ptrtoint (i8* @FoldBomb to i64) to i128)
@const_fptrunc = global float fptrunc (double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double) to float)
@const_fpext = global fp128 fpext (double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double) to fp128)
@const_uitofp = global double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double)
@const_sitofp = global double sitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double)
@const_fptoui = global i32 fptoui (double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double) to i32)
@const_fptosi = global i32 fptosi (double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double) to i32)
@const_ptrtoint = global i32 ptrtoint (i8* getelementptr (i8* null, i32 1) to i32)
@const_inttoptr = global i8* inttoptr (i64 add (i64 ptrtoint (i8* @FoldBomb to i64), i64 5) to i8*)
@const_bitcast = global i64 bitcast (double uitofp (i64 ptrtoint (i8* @FoldBomb to i64) to double) to i64)
@const_size_of = global i64 ptrtoint (i1** getelementptr (i1** null, i32 1) to i64)
@const_gep = global i8* getelementptr (i8* @FoldBomb, i64 5)
@const_select = global i8 select (i1 icmp sle (i64 ptrtoint (i8* @FoldBomb to i64), i64 5), i8 -1, i8 0)
@const_extractelement = global i32 extractelement (<4 x i32> <i32 0, i32 1, i32 0, i32 1>, i32 ptrtoint (i8* @FoldBomb to i32))
@const_insertelement = global <4 x i32> insertelement (<4 x i32> <i32 0, i32 1, i32 0, i32 1>, i32 0, i32 ptrtoint (i8* @FoldBomb to i32))
@const_shufflevector = global <4 x i32> <i32 0, i32 1, i32 1, i32 0>
@GVal01 = global i32 0
@GVal02 = linkonce global i32 0
@GVal03 = global i32 0, section "Hanalei"
@GVal04 = hidden global i32 0
@GVal05 = global i32 0, align 128
@GVar01 = external global i32
@QGVar01 = external global i32
@GVar02 = global i32 42
@GVar03 = global i32 42
@QGVar02 = addrspace(3) global i32 42
@QGVar03 = global i32 42
@GVar04 = thread_local global i32 42
@ConstGlobalVar = constant i32 42
@aliasee = external global i32

@alias = alias i32* @aliasee

define i32 @use_function(i32, i32) {
entry:
  %v1 = add i32 %0, %1
  %v2 = add i32 %0, %v1
  %v3 = add i32 %v1, %v2
  unreachable
}

define i32 @user_function(i32, i32) {
entry:
  %user_alloca = alloca i32
  %user_load = load i32* %user_alloca
  %sum = add i32 %0, %user_load
  unreachable
}

declare i32 @Fn1(i32, i64)

define i32 @Fn3(i32, i64) {
entry:
  unreachable
}

define i32 @Fn4(i32 %Param1, i64 %Param2) {
entry:
  unreachable
}

define fastcc i32 @Fn5(i32, i64) {
entry:
  unreachable
}

define i32 @Fn6(i32, i64) gc "shadowstack" {
entry:
  unreachable
}

define void @X() {
Bb1:
  unreachable
}

declare void @X2()

define void @X3() {
a:
  unreachable

b:                                                ; No predecessors!
  unreachable
}

define void @X4() {
Bb3:
  unreachable
}

define void @X5() {
entry:
  unreachable
}

define void @BuilderParent(i32) {
entry:
  %sum = add i32 %0, %0
  ret void
}

define void @X6() {
Bb01:
  ret void
}

define i32 @X7(i32 %P1, i32 %P2) {
entry:
  %F1 = uitofp i32 %P1 to float
  %F2 = uitofp i32 %P2 to float
  %build_trunc = trunc i32 %P1 to i8
  %build_zext = zext i8 %build_trunc to i32
  %build_sext = sext i32 %build_zext to i64
  %build_uitofp = uitofp i64 %build_sext to float
  %build_sitofp = sitofp i32 %build_zext to double
  %build_fptoui = fptoui float %build_uitofp to i32
  %build_fptosi = fptosi double %build_sitofp to i64
  %build_fptrunc = fptrunc double %build_sitofp to float
  %build_fpext = fpext float %build_fptrunc to double
  %build_inttoptr = inttoptr i32 %P1 to i8*
  %build_ptrtoint = ptrtoint i8* %build_inttoptr to i64
  %build_bitcast = bitcast i64 %build_ptrtoint to double
  %build_icmp_ne = icmp ne i32 %P1, %P2
  %build_icmp_sle = icmp sle i32 %P2, %P1
  %build_fcmp_false = fcmp false float %F1, %F2
  %build_fcmp_true = fcmp true float %F2, %F1
  %build_call = tail call cc63 i32 @X7(i32 signext %P2, i32 %P1)
  %build_icmp = icmp eq i32 %P1, %P2
  %build_select = select i1 %build_icmp, i32 %P1, i32 %P2
  %build_va_arg = va_arg i8** null, i32
  %Vec1 = insertelement <4 x i32> <i32 1, i32 0, i32 1, i32 0>, i32 %P1, i32 %P2
  %Vec2 = insertelement <4 x i32> <i32 0, i32 1, i32 0, i32 1>, i32 %P1, i32 %P2
  %build_extractelement = extractelement <4 x i32> %Vec1, i32 %P2
  %build_insertelement = insertelement <4 x i32> %Vec1, i32 %P1, i32 %P2
  %build_shufflevector = shufflevector <4 x i32> %Vec1, <4 x i32> %Vec2, <4 x i32> <i32 1, i32 1, i32 0, i32 0>
  %metadata = add i32 %P1, %P2, !test !0
  %dbg = add i32 %P1, %P2, !dbg !1
  ret i32 %P1

Bb00:                                             ; preds = %Bb04, %Bb03
  unreachable

Bb02:                                             ; preds = %Bb02
  br label %Bb02

Bb03:                                             ; preds = %Bb03
  %build_br = trunc i32 %P1 to i1
  br i1 %build_br, label %Bb03, label %Bb00

SwiBlock1:                                        ; No predecessors!
  switch i32 %P1, label %SwiBlock3 [
    i32 2, label %SwiBlock2
  ]

SwiBlock2:                                        ; preds = %SwiBlock1
  unreachable

SwiBlock3:                                        ; preds = %SwiBlock1
  unreachable

IBRBlock1:                                        ; No predecessors!
  indirectbr i8* blockaddress(@X7, %IBRBlock2), [label %IBRBlock2, label %IBRBlock3]

IBRBlock2:                                        ; preds = %IBRBlock1
  unreachable

IBRBlock3:                                        ; preds = %IBRBlock1
  unreachable

Bb04:                                             ; preds = %Bb04
  %build_invoke = invoke i32 @X7(i32 %P1, i32 %P2)
          to label %Bb04 unwind label %Bb00

Bb05:                                             ; No predecessors!
  unwind

Bb06:                                             ; No predecessors!
  unreachable

Bb07:                                             ; No predecessors!
  %build_add = add i32 %P1, %P2
  %build_nsw_add = add nsw i32 %P1, %P2
  %build_nuw_add = add nuw i32 %P1, %P2
  %build_fadd = fadd float %F1, %F2
  %build_sub = sub i32 %P1, %P2
  %build_nsw_sub = sub nsw i32 %P1, %P2
  %build_nuw_sub = sub nuw i32 %P1, %P2
  %build_fsub = fsub float %F1, %F2
  %build_mul = mul i32 %P1, %P2
  %build_nsw_mul = mul nsw i32 %P1, %P2
  %build_nuw_mul = mul nuw i32 %P1, %P2
  %build_fmul = fmul float %F1, %F2
  %build_udiv = udiv i32 %P1, %P2
  %build_sdiv = sdiv i32 %P1, %P2
  %build_exact_sdiv = sdiv exact i32 %P1, %P2
  %build_fdiv = fdiv float %F1, %F2
  %build_urem = urem i32 %P1, %P2
  %build_srem = srem i32 %P1, %P2
  %build_frem = frem float %F1, %F2
  %build_shl = shl i32 %P1, %P2
  %build_lshl = lshr i32 %P1, %P2
  %build_ashl = ashr i32 %P1, %P2
  %build_and = and i32 %P1, %P2
  %build_or = or i32 %P1, %P2
  %build_xor = xor i32 %P1, %P2
  %build_neg = sub i32 0, %P1
  %build_nsw_neg = sub nsw i32 0, %P1
  %build_nuw_neg = sub nuw i32 0, %P1
  %build_fneg = fsub float -0.000000e+00, %F1
  %build_not = xor i32 %P1, -1
  unreachable

Bb08:                                             ; No predecessors!
  %build_alloca = alloca i32
  %build_array_alloca = alloca i32, i32 %P2
  %build_load = load i32* %build_array_alloca
  store i32 %P2, i32* %build_alloca
  %build_gep = getelementptr i32* %build_array_alloca, i32 %P2
  unreachable

PhiBlock1:                                        ; No predecessors!
  br label %PhiJoinBlock

PhiBlock2:                                        ; No predecessors!
  br label %PhiJoinBlock

PhiJoinBlock:                                     ; preds = %PhiBlock2, %PhiBlock1
  %PhiNode = phi i32 [ %P1, %PhiBlock1 ], [ %P2, %PhiBlock2 ]
  unreachable
}

define void @FunctionPassManager() {
entry:
  ret void
}

!0 = metadata !{i32 1, metadata !"metadata test"}
!1 = metadata !{i32 2, i32 3, metadata !2, metadata !2}
!2 = metadata !{}

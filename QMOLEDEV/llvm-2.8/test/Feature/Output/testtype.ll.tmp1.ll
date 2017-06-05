; ModuleID = '<stdin>'

%0 = type { i8 }
%X = type i32* addrspace(4)*
%fwd = type %fwdref*
%fwdref = type { %fwd* }
%inners = type { float, %0 }
%struct = type { i32, %inners, i64 }
%test = type %fwdref*
%test2 = type [2 x i32]
%test3 = type i32 (i32 ()*, float (...)*, ...)*

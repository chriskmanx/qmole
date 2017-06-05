; ModuleID = '<stdin>'

%0 = type { i8 }
%inners = type { float, %0 }
%struct = type { i32, %inners, i64 }

define i32 @testfunction(i32 %i0, i32 %j0) {
  %1 = alloca i8, i32 5
  %ptr = alloca i32
  store i32 3, i32* %ptr
  %val = load i32* %ptr
  %sptr = alloca %struct
  %nsptr = getelementptr %struct* %sptr, i64 0, i32 1
  %ubsptr = getelementptr %inners* %nsptr, i64 0, i32 1
  %idx = getelementptr %0* %ubsptr, i64 0, i32 0
  store i8 4, i8* %idx
  %fptr = getelementptr %struct* %sptr, i64 0, i32 1, i32 0
  store float 4.000000e+00, float* %fptr
  ret i32 3
}

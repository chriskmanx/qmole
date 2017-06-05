; ModuleID = '<stdin>'

%0 = type { i8 }
%inners = type { float, %0 }
%struct = type { i33, %inners, i64 }

define i33 @testfunction(i33 %i0, i33 %j0) {
  %1 = alloca i8, i32 5
  %ptr = alloca i33
  store i33 3, i33* %ptr
  %val = load i33* %ptr
  %sptr = alloca %struct
  %nsptr = getelementptr %struct* %sptr, i64 0, i32 1
  %ubsptr = getelementptr %inners* %nsptr, i64 0, i32 1
  %idx = getelementptr %0* %ubsptr, i64 0, i32 0
  store i8 4, i8* %idx
  %fptr = getelementptr %struct* %sptr, i64 0, i32 1, i32 0
  store float 4.000000e+00, float* %fptr
  ret i33 3
}

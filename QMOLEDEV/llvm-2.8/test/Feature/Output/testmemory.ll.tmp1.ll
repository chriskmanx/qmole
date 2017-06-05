; ModuleID = '<stdin>'

%0 = type { [4 x i8*], float }
%1 = type { float, %2 }
%2 = type { i8 }
%complexty = type { i32, %0, double }
%struct = type { i32, %1, i64 }

define i32 @main() {
  %1 = call i32 @testfunction(i64 0, i64 1)
  ret i32 0
}

define i32 @testfunction(i64 %i0, i64 %j0) {
  %malloccall = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i8* getelementptr (i8* null, i32 1) to i64), i64 4) to i32))
  %array0 = bitcast i8* %malloccall to [4 x i8]*
  %size = add i32 2, 2
  %array1 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i8* getelementptr (i8* null, i32 1) to i32), i32 4))
  %mallocsize = mul i32 %size, ptrtoint (i8* getelementptr (i8* null, i32 1) to i32)
  %array2 = tail call i8* @malloc(i32 %mallocsize)
  %idx = getelementptr [4 x i8]* %array0, i64 0, i64 2
  store i8 123, i8* %idx
  %1 = bitcast [4 x i8]* %array0 to i8*
  tail call void @free(i8* %1)
  tail call void @free(i8* %array1)
  tail call void @free(i8* %array2)
  %aa = alloca %complexty, i32 5
  %idx2 = getelementptr %complexty* %aa, i64 %i0, i32 1, i32 0, i64 %j0
  store i8* null, i8** %idx2
  %ptr = alloca i32
  store i32 3, i32* %ptr
  %val = load i32* %ptr
  %sptr = alloca %struct
  %ubsptr = getelementptr %struct* %sptr, i64 0, i32 1, i32 1
  %idx3 = getelementptr %2* %ubsptr, i64 0, i32 0
  store i8 4, i8* %idx3
  ret i32 3
}

declare noalias i8* @malloc(i32)

declare void @free(i8*)

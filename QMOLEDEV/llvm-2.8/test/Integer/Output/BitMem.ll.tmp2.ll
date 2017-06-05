; ModuleID = '<stdin>'

define void @foo() {
  %malloccall = tail call i8* @malloc(i32 mul (i32 ptrtoint (i31* getelementptr (i31* null, i32 1) to i32), i32 4))
  %t1 = bitcast i8* %malloccall to i31*
  %malloccall1 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i31* getelementptr (i31* null, i32 1) to i32), i32 7))
  %t2 = bitcast i8* %malloccall1 to i31*
  %malloccall2 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i15* getelementptr (i15* null, i32 1) to i64), i64 4) to i32))
  %t3 = bitcast i8* %malloccall2 to [4 x i15]*
  %idx = getelementptr [4 x i15]* %t3, i64 0, i64 2
  store i15 -123, i15* %idx
  %1 = bitcast [4 x i15]* %t3 to i8*
  tail call void @free(i8* %1)
  %2 = bitcast i31* %t2 to i8*
  tail call void @free(i8* %2)
  %3 = bitcast i31* %t1 to i8*
  tail call void @free(i8* %3)
  %t4 = alloca i12, i32 100
  %4 = bitcast i12* %t4 to i8*
  tail call void @free(i8* %4)
  %t5 = alloca i31
  store i31 -123, i31* %t5
  %5 = bitcast i31* %t5 to i8*
  tail call void @free(i8* %5)
  ret void
}

declare noalias i8* @malloc(i32)

declare void @free(i8*)

; ModuleID = '<stdin>'

@X = global i32 4, align 16

define i32* @test() align 32 {
  %X = alloca i32, align 4
  %Y = alloca i32, i32 42, align 16
  %Z = alloca i32
  ret i32* %X
}

define i32* @test2() {
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32* null, i32 1) to i32))
  %X = bitcast i8* %malloccall to i32*
  %malloccall1 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i32* getelementptr (i32* null, i32 1) to i32), i32 42))
  %Y = bitcast i8* %malloccall1 to i32*
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32* null, i32 1) to i32))
  %Z = bitcast i8* %malloccall2 to i32*
  %malloccall3 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32* null, i32 1) to i32))
  %T = bitcast i8* %malloccall3 to i32*
  ret i32* %X
}

declare noalias i8* @malloc(i32)

define void @test3() alignstack(16) {
  ret void
}

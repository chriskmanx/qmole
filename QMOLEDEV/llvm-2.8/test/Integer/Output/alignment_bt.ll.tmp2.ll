; ModuleID = '<stdin>'

@X = global i19 4, align 16

define i19* @test() align 32 {
  %X = alloca i19, align 4
  %Y = alloca i51, i32 42, align 16
  %Z = alloca i32, align 1
  ret i19* %X
}

define i19* @test2() {
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i19* getelementptr (i19* null, i32 1) to i32))
  %X = bitcast i8* %malloccall to i19*
  %malloccall1 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i51* getelementptr (i51* null, i32 1) to i32), i32 42))
  %Y = bitcast i8* %malloccall1 to i51*
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32* null, i32 1) to i32))
  %Z = bitcast i8* %malloccall2 to i32*
  ret i19* %X
}

declare noalias i8* @malloc(i32)

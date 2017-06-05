; ModuleID = '<stdin>'

%FunTy = type i31 (i31)

define void @invoke(%FunTy* %x) {
  %foo = call i31 %x(i31 123)
  ret void
}

define i31 @main(i31 %argc, i8** %argv, i8** %envp) {
  %retval = call i31 @test(i31 %argc)
  %two = add i31 %retval, %retval
  %retval2 = call i31 @test(i31 %argc)
  %two2 = add i31 %two, %retval2
  call void @invoke(%FunTy* @test)
  ret i31 %two2
}

define i31 @test(i31 %i0) {
  ret i31 %i0
}

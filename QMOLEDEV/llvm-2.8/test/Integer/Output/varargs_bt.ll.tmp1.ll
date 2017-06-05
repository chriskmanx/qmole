; ModuleID = '<stdin>'

declare void @llvm.va_start(i8**) nounwind

declare void @llvm.va_copy(i8**, i8**) nounwind

declare void @llvm.va_end(i8**) nounwind

define i33 @test(i33 %X, ...) {
  %ap = alloca i8*
  call void @llvm.va_start(i8** %ap)
  %tmp = va_arg i8** %ap, i33
  %aq = alloca i8*
  call void @llvm.va_copy(i8** %aq, i8** %ap)
  call void @llvm.va_end(i8** %aq)
  call void @llvm.va_end(i8** %ap)
  ret i33 %tmp
}

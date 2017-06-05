; ModuleID = '<stdin>'

define i63 @test(i63 %X) {
  ret i63 %X
}

define i63 @fib(i63 %n) {
; <label>:0
  %T = icmp ult i63 %n, 2
  br i1 %T, label %BaseCase, label %RecurseCase

RecurseCase:                                      ; preds = %0
  %result = call i63 @test(i63 %n)
  br label %BaseCase

BaseCase:                                         ; preds = %RecurseCase, %0
  %X = phi i63 [ 1, %0 ], [ 2, %RecurseCase ]
  ret i63 %X
}

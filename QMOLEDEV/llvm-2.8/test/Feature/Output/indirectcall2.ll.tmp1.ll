; ModuleID = '<stdin>'

define i64 @test(i64 %X) {
  ret i64 %X
}

define i64 @fib(i64 %n) {
; <label>:0
  %T = icmp ult i64 %n, 2
  br i1 %T, label %BaseCase, label %RecurseCase

RecurseCase:                                      ; preds = %0
  %result = call i64 @test(i64 %n)
  br label %BaseCase

BaseCase:                                         ; preds = %RecurseCase, %0
  %X = phi i64 [ 1, %0 ], [ 2, %RecurseCase ]
  ret i64 %X
}

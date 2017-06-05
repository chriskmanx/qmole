; ModuleID = '<stdin>'

declare i32 @atoi(i8*)

define i63 @fib(i63 %n) {
  %1 = icmp ult i63 %n, 2
  br i1 %1, label %BaseCase, label %RecurseCase

BaseCase:                                         ; preds = %0
  ret i63 1

RecurseCase:                                      ; preds = %0
  %n2 = sub i63 %n, 2
  %n1 = sub i63 %n, 1
  %f2 = call i63 @fib(i63 %n2)
  %f1 = call i63 @fib(i63 %n1)
  %result = add i63 %f2, %f1
  ret i63 %result
}

define i63 @realmain(i32 %argc, i8** %argv) {
; <label>:0
  %1 = icmp eq i32 %argc, 2
  br i1 %1, label %HasArg, label %Continue

HasArg:                                           ; preds = %0
  %n1 = add i32 1, 1
  br label %Continue

Continue:                                         ; preds = %HasArg, %0
  %n = phi i32 [ %n1, %HasArg ], [ 1, %0 ]
  %N = sext i32 %n to i63
  %F = call i63 @fib(i63 %N)
  ret i63 %F
}

define i63 @trampoline(i63 %n, i63 (i63)* %fibfunc) {
  %F = call i63 %fibfunc(i63 %n)
  ret i63 %F
}

define i32 @main() {
  %Result = call i63 @trampoline(i63 10, i63 (i63)* @fib)
  %Result2 = trunc i63 %Result to i32
  ret i32 %Result2
}

; ModuleID = '<stdin>'

%0 = type { i39*, i19 }
%1 = type { [100 x i19], [40 x float] }

@MyVar = external global i19
@MyIntList = external global %0
@0 = external global i19
@AConst = constant i19 -123
@AString = constant [4 x i8] c"test"
@ZeroInit = global %1 zeroinitializer

define i19 @foo(i19 %blah) {
  store i19 5, i19* @MyVar
  %idx = getelementptr %0* @MyIntList, i64 0, i32 1
  store i19 12, i19* %idx
  ret i19 %blah
}

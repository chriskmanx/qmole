; ModuleID = '<stdin>'

%0 = type { %0*, i27 }
%1 = type { [100 x i27], [40 x float] }

@MyVar = external global i27
@MyIntList = external global %0
@0 = external global i27
@AConst = constant i27 123
@AString = constant [4 x i8] c"test"
@ZeroInit = global %1 zeroinitializer

define i27 @foo(i27 %blah) {
  store i27 5, i27* @MyVar
  %idx = getelementptr %0* @MyIntList, i64 0, i32 1
  store i27 12, i27* %idx
  ret i27 %blah
}

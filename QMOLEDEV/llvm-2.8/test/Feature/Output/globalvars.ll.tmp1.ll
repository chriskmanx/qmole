; ModuleID = '<stdin>'

%0 = type { %0*, i32 }
%1 = type { [100 x i32], [40 x float] }

@MyVar = external global i32
@MyIntList = external global %0
@0 = external global i32
@AConst = constant i32 123
@AString = constant [4 x i8] c"test"
@ZeroInit = global %1 zeroinitializer

define i32 @foo(i32 %blah) {
  store i32 5, i32* @MyVar
  %idx = getelementptr %0* @MyIntList, i64 0, i32 1
  store i32 12, i32* %idx
  ret i32 %blah
}

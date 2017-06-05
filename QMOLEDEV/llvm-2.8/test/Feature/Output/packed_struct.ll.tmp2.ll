; ModuleID = '<stdin>'

%0 = type <{ i32, i8 }>
%1 = type <{ i8, i32, i32 }>
%2 = type { i8, i32, i32 }
%struct.anon = type <{ i8, i32, i32, i32 }>

@foos = external global %struct.anon
@bara = external global [2 x %0]
@E1 = global %1 <{ i8 1, i32 2, i32 3 }>
@E2 = global %2 { i8 4, i32 5, i32 6 }

define i32 @main() {
  %tmp = load i32* getelementptr inbounds (%struct.anon* @foos, i32 0, i32 1)
  %tmp3 = load i32* getelementptr inbounds (%struct.anon* @foos, i32 0, i32 2)
  %tmp6 = load i32* getelementptr inbounds (%struct.anon* @foos, i32 0, i32 3)
  %tmp4 = add i32 %tmp3, %tmp
  %tmp7 = add i32 %tmp4, %tmp6
  ret i32 %tmp7
}

define i32 @bar() {
entry:
  %tmp = load i32* getelementptr inbounds ([2 x %0]* @bara, i32 0, i32 0, i32 0)
  %tmp4 = load i32* getelementptr inbounds ([2 x %0]* @bara, i32 0, i32 1, i32 0)
  %tmp5 = add i32 %tmp4, %tmp
  ret i32 %tmp5
}

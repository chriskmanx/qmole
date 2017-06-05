; ModuleID = '<stdin>'

%0 = type <{ i35, i8 }>
%1 = type <{ i8, i35, i35 }>
%2 = type { i8, i35, i35 }
%struct.anon = type <{ i8, i35, i35, i35 }>

@foos = external global %struct.anon
@bara = external global [2 x %0]
@E1 = global %1 <{ i8 1, i35 2, i35 3 }>
@E2 = global %2 { i8 4, i35 5, i35 6 }

define i35 @main() {
  %tmp = load i35* getelementptr inbounds (%struct.anon* @foos, i32 0, i32 1)
  %tmp3 = load i35* getelementptr inbounds (%struct.anon* @foos, i32 0, i32 2)
  %tmp6 = load i35* getelementptr inbounds (%struct.anon* @foos, i32 0, i32 3)
  %tmp4 = add i35 %tmp3, %tmp
  %tmp7 = add i35 %tmp4, %tmp6
  ret i35 %tmp7
}

define i35 @bar() {
entry:
  %tmp = load i35* getelementptr inbounds ([2 x %0]* @bara, i32 0, i32 0, i32 0)
  %tmp4 = load i35* getelementptr inbounds ([2 x %0]* @bara, i32 0, i32 1, i32 0)
  %tmp5 = add i35 %tmp4, %tmp
  ret i35 %tmp5
}

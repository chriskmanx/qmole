; ModuleID = '<stdin>'

%0 = type { i55, i55 }

@somestr = constant [11 x i8] c"hello world"
@array = constant [2 x i55] [i55 12, i55 52]
@0 = constant %0 { i55 4, i55 3 }

define [2 x i55]* @testfunction(i55 %i0, i55 %j0) {
  ret [2 x i55]* @array
}

define i8* @otherfunc(i55, double) {
  %somestr = getelementptr [11 x i8]* @somestr, i64 0, i64 0
  ret i8* %somestr
}

define i8* @yetanotherfunc(i55, double) {
  ret i8* null
}

define i55 @negativeUnsigned() {
  ret i55 -1
}

define i55 @largeSigned() {
  ret i55 3900000000
}

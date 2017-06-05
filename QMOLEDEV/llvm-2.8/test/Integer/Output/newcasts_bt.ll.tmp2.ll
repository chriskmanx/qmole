; ModuleID = '<stdin>'

define void @NewCasts(i17 %x) {
  %a = zext i17 %x to i32
  %b = sext i17 %x to i32
  %c = trunc i17 %x to i8
  %d = uitofp i17 %x to float
  %e = sitofp i17 %x to double
  %f = fptoui float %d to i17
  %g = fptosi double %e to i17
  %i = fpext float %d to double
  %j = fptrunc double %i to float
  %k = bitcast i32 %a to float
  %l = inttoptr i17 %x to i32*
  %m = ptrtoint i32* %l to i64
  ret void
}

define i17 @ZExtConst() {
  ret i17 42
}

define i17 @SExtConst() {
  ret i17 42
}

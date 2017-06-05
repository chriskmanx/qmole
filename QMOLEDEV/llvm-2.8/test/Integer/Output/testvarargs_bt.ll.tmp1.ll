; ModuleID = '<stdin>'

declare i31 @printf(i8*, ...)

define i31 @testvarar() {
  %1 = call i31 (i8*, ...)* @printf(i8* null, i31 12, i8 42)
  ret i31 %1
}

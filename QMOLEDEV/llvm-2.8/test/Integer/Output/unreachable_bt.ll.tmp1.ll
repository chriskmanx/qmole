; ModuleID = '<stdin>'

declare void @bar()

define i9 @foo() {
  unreachable
}

define double @xyz() {
  call void @bar()
  unreachable
}

; ModuleID = '<stdin>'

%i35 = type i35

define i35 @squared(i35 %i0) {
  switch i35 %i0, label %Default [
    i35 1, label %Case1
    i35 2, label %Case2
    i35 4, label %Case4
  ]

Default:                                          ; preds = %0
  ret i35 -1

Case1:                                            ; preds = %0
  ret i35 1

Case2:                                            ; preds = %0
  ret i35 4

Case4:                                            ; preds = %0
  ret i35 16
}

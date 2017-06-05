; ModuleID = '<stdin>'

%int = type i32

@Addr = global i8* blockaddress(@indbrtest, %BB1)
@Addr3 = global i8* blockaddress(@squared, %Case1)

define i32 @squared(i32 %i0) {
  switch i32 %i0, label %Default [
    i32 1, label %Case1
    i32 2, label %Case2
    i32 4, label %Case4
  ]

Default:                                          ; preds = %0
  ret i32 -1

Case1:                                            ; preds = %0
  ret i32 1

Case2:                                            ; preds = %0
  ret i32 4

Case4:                                            ; preds = %0
  ret i32 16
}

define i32 @indbrtest(i8* %P, i32* %Q) {
  indirectbr i8* %P, [label %BB1, label %BB2, label %BB3]

BB1:                                              ; preds = %BB2, %0
  indirectbr i32* %Q, []

BB2:                                              ; preds = %BB2, %0
  %R = bitcast i8* blockaddress(@indbrtest, %BB3) to i8*
  indirectbr i8* %R, [label %BB1, label %BB2, label %BB3]

BB3:                                              ; preds = %BB2, %0
  ret i32 2
}

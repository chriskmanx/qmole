; ModuleID = '<stdin>'

define i55 @simpleIcmp(i55 %i0, i55 %j0) {
  %t1 = icmp eq i55 %i0, %j0
  %t2 = icmp ne i55 %i0, %j0
  %t3 = icmp ult i55 %i0, %j0
  %t4 = icmp sgt i55 %i0, %j0
  %t5 = icmp ule i55 %i0, %j0
  %t6 = icmp sge i55 %i0, %j0
  %t7 = icmp eq i55 %i0, 1098765432
  %t8 = icmp ne i55 %i0, -31415926
  %t9 = icmp ult i55 10000, %j0
  %t10 = icmp sgt i55 -10000, %j0
  ret i55 %i0
}

define i31 @phitest(i12 %i) {
HasArg:
  %n1 = add i12 1, %i
  br label %Continue

Continue:                                         ; preds = %Continue, %HasArg
  %n = phi i12 [ %n1, %HasArg ], [ %next, %Continue ]
  %next = add i12 1, %n
  br label %Continue
}

define i18 @select(i18 %i) {
  %t = icmp sgt i18 %i, 100
  %k = select i1 %t, i18 %i, i18 999
  ret i18 %k
}

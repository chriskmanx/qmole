; ModuleID = '<stdin>'

define i31 @simpleArith(i31 %i0, i31 %j0) {
  %t1 = add i31 %i0, %j0
  %t2 = sub i31 %i0, %j0
  %t3 = mul i31 %t1, %t2
  %t4 = udiv i31 %t1, %t2
  %t5 = sdiv i31 %t1, %t2
  %t6 = urem i31 %t1, %t2
  %t7 = srem i31 %t1, %t2
  %t8 = shl i31 %t1, 9
  %t9 = lshr i31 %t1, 9
  %t10 = ashr i31 %t1, 9
  %f1 = sitofp i31 %t1 to float
  %f2 = fdiv float 4.000000e+00, %f1
  ret i31 %t3
}

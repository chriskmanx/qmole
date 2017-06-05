; ModuleID = '<stdin>'

define i31 @simpleAdd(i31 %i0, i31 %j0) {
  %t1 = xor i31 %i0, %j0
  %t2 = or i31 %i0, %j0
  %t3 = and i31 %t1, %t2
  %t4 = shl i31 %i0, 2
  %t5 = ashr i31 %i0, 2
  %t6 = lshr i31 %j0, 22
  ret i31 %t3
}

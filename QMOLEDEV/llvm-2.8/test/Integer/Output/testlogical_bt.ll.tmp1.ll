; ModuleID = '<stdin>'

define i31 @simpleAdd(i31 %i0, i31 %j0) {
  %t1 = xor i31 %i0, %j0
  %t2 = or i31 %i0, %j0
  %t3 = and i31 %t1, %t2
  ret i31 %t3
}

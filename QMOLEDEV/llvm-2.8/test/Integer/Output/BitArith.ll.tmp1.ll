; ModuleID = '<stdin>'

define void @foo(i31 %i, i63 %j, i10 %k) {
  %t1 = trunc i63 %j to i31
  %t2 = add i31 %t1, %i
  %t20 = add i31 3, %t1
  %t3 = zext i31 %i to i63
  %t4 = sub i63 %t3, %j
  %t40 = sub i63 %j, -100
  %t5 = mul i10 %k, 7
  %t6 = sdiv i63 %j, -2
  %t7 = udiv i63 %j, %t3
  %t8 = urem i10 %k, 10
  %t9 = srem i10 %k, -10
  ret void
}

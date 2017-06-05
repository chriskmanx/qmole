; ModuleID = '<stdin>'

define void @foo(i31 %i, i1280 %j, i1 %k, float %f) {
  %t1 = trunc i1280 %j to i31
  %t2 = trunc i31 %t1 to i1
  %t3 = zext i31 %i to i1280
  %t4 = sext i31 %i to i1280
  %t5 = fptoui float 0x400921FA00000000 to i31
  %t6 = uitofp i31 %t5 to double
  %t7 = fptosi double 0xC0934A456D5CFAAD to i28
  %t8 = sitofp i8 -1 to double
  %t9 = uitofp i8 -1 to double
  ret void
}

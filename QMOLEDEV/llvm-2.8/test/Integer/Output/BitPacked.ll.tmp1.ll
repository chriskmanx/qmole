; ModuleID = '<stdin>'

@foo1 = external global <4 x float>
@foo2 = external global <2 x i10>

define void @main() {
  store <4 x float> <float 1.000000e+00, float 2.000000e+00, float 3.000000e+00, float 4.000000e+00>, <4 x float>* @foo1
  store <2 x i10> <i10 4, i10 4>, <2 x i10>* @foo2
  %l1 = load <4 x float>* @foo1
  %l2 = load <2 x i10>* @foo2
  %r1 = extractelement <2 x i10> %l2, i32 1
  %r2 = extractelement <2 x i10> %l2, i32 0
  %t = mul i10 %r1, %r2
  %r3 = insertelement <2 x i10> %l2, i10 %t, i32 0
  store <2 x i10> %r3, <2 x i10>* @foo2
  ret void
}

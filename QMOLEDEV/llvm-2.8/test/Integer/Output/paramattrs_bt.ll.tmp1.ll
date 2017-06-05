; ModuleID = '<stdin>'

%SFunTy = type i33 (i8)
%ZFunTy = type i33 (i8)

declare signext i16 @test(i16 signext)

declare zeroext i8 @test2(i16 zeroext)

define i33 @main(i33 %argc, i8** %argv) {
  %val = trunc i33 %argc to i16
  %res = call signext i16 @test(i16 signext %val)
  %two = add i16 %res, %res
  %res2 = call zeroext i8 @test2(i16 zeroext %two)
  %retVal = sext i16 %two to i33
  ret i33 %retVal
}

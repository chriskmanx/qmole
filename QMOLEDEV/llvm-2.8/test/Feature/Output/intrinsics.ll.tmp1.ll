; ModuleID = '<stdin>'

declare i1 @llvm.isunordered.f32(float, float)

declare i1 @llvm.isunordered.f64(double, double)

declare void @llvm.prefetch(i8* nocapture, i32, i32) nounwind

declare i8 @llvm.ctpop.i8(i8) nounwind readnone

declare i16 @llvm.ctpop.i16(i16) nounwind readnone

declare i32 @llvm.ctpop.i32(i32) nounwind readnone

declare i64 @llvm.ctpop.i64(i64) nounwind readnone

declare i8 @llvm.cttz.i8(i8) nounwind readnone

declare i16 @llvm.cttz.i16(i16) nounwind readnone

declare i32 @llvm.cttz.i32(i32) nounwind readnone

declare i64 @llvm.cttz.i64(i64) nounwind readnone

declare i8 @llvm.ctlz.i8(i8) nounwind readnone

declare i16 @llvm.ctlz.i16(i16) nounwind readnone

declare i32 @llvm.ctlz.i32(i32) nounwind readnone

declare i64 @llvm.ctlz.i64(i64) nounwind readnone

declare float @llvm.sqrt.f32(float) nounwind readonly

declare double @llvm.sqrt.f64(double) nounwind readonly

define void @libm() {
  %1 = fcmp uno float 1.000000e+00, 2.000000e+00
  %2 = fcmp uno double 3.000000e+00, 4.000000e+00
  call void @llvm.prefetch(i8* null, i32 1, i32 3)
  %3 = call float @llvm.sqrt.f32(float 5.000000e+00)
  %4 = call double @llvm.sqrt.f64(double 6.000000e+00)
  %5 = call i8 @llvm.ctpop.i8(i8 10)
  %6 = call i16 @llvm.ctpop.i16(i16 11)
  %7 = call i32 @llvm.ctpop.i32(i32 12)
  %8 = call i64 @llvm.ctpop.i64(i64 13)
  %9 = call i8 @llvm.ctlz.i8(i8 14)
  %10 = call i16 @llvm.ctlz.i16(i16 15)
  %11 = call i32 @llvm.ctlz.i32(i32 16)
  %12 = call i64 @llvm.ctlz.i64(i64 17)
  %13 = call i8 @llvm.cttz.i8(i8 18)
  %14 = call i16 @llvm.cttz.i16(i16 19)
  %15 = call i32 @llvm.cttz.i32(i32 20)
  %16 = call i64 @llvm.cttz.i64(i64 21)
  ret void
}

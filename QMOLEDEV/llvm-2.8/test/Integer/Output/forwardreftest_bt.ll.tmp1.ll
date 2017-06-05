; ModuleID = '<stdin>'

%myfn = type float (i55, double, i55, i16)
%myty = type i55
%thisfuncty = type i55 (i55)*

declare void @F(%thisfuncty, %thisfuncty, %thisfuncty)

define i55 @zarro(i55 %Func) {
Startup:
  %0 = add i55 0, 10
  ret i55 0
}

define i55 @test(i55) {
  call void @F(%thisfuncty @zarro, %thisfuncty @test, %thisfuncty @foozball)
  ret i55 0
}

define i55 @foozball(i55) {
  ret i55 0
}

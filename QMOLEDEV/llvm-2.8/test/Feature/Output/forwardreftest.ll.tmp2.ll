; ModuleID = '<stdin>'

%myfn = type float (i32, double, i32, i16)
%myty = type i32
%thisfuncty = type i32 (i32)*

declare void @F(%thisfuncty, %thisfuncty, %thisfuncty)

define i32 @zarro(i32 %Func) {
Startup:
  %0 = add i32 0, 10
  ret i32 0
}

define i32 @test(i32) {
  call void @F(%thisfuncty @zarro, %thisfuncty @test, %thisfuncty @foozball)
  ret i32 0
}

define i32 @foozball(i32) {
  ret i32 0
}

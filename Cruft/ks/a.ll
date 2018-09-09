; ModuleID = 'Luttu'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i32, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %abcd_result = call i32 @abcd(i32 99)
  ret i32 %abcd_result
  %printbig = call i32 @printbig(i32 448888)
  ret i32 0
}

define i32 @abcd(i32 %c) {
entry:
  %c1 = alloca i32
  store i32 %c, i32* %c1
  %c2 = alloca i32
  %c3 = load i32, i32* %c2
  ret i32 %c3
}

define i32 @abc() {
entry:
  ret i32 78
}

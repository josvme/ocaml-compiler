; ModuleID = 'Luttu'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i32, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %abc_result = call i32 @abc()
  ret i32 %abc_result
}

define i32 @bcd() {
entry:
  %abc_result = call i32 @abc()
  ret i32 0
}

define i32 @abc() {
entry:
  ret i32 24
}

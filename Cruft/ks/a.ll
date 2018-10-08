; ModuleID = 'Luttu'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i32, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %printbig = call i32 @printbig(i32 10)
  %k = alloca i32
  store i32 2000, i32* %k
  %k1 = load i32, i32* %k
  %printbig2 = call i32 @printbig(i32 %k1)
  %abcd_result = call i32 @abcd(i32 3)
  %printbig3 = call i32 @printbig(i32 %abcd_result)
  %j = alloca i32
  %abc_result = call i32 @abc()
  store i32 %abc_result, i32* %j
  %j4 = load i32, i32* %j
  %printbig5 = call i32 @printbig(i32 %j4)
  ret i32 50
}

define i32 @abcd(i32 %c) {
entry:
  %c1 = alloca i32
  store i32 %c, i32* %c1
  %c2 = load i32, i32* %c1
  %tmp = mul i32 %c2, 1000
  ret i32 %tmp
}

define i32 @abc() {
entry:
  ret i32 2020
}

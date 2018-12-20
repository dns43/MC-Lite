; ModuleID = 'MC-Lite'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

define void @main() {
entry:
  %x = alloca i64
  store i64 10, i64* %x
  %y = alloca i64
  store i64 5, i64* %y
  %a = load i64* %x
  %b = load i64* %y
  %c = add i64 1, %a
  ret void
}

declare i64 @printf(i8*, ...)

; vim: set ft=llvm:
target triple = "x86_64-unknown-linux-gnu"

define private i64 @fact_helper(i64 %n, i64 %acc) {
entry:
  %0 = icmp sle i64 %n, 1
  br i1 %0, label %le1, label %gt1
le1:
  ret i64 %acc
gt1:
  %acc1 = mul nsw i64 %acc, %n
  %n1 = sub nsw i64 %n, 1
  %res = musttail call i64 @fact_helper(i64 %n1, i64 %acc1)
  ret i64 %res
}

define private i64 @fact(i64 %n) {
  %res = call i64 @fact_helper(i64 %n, i64 1)
  ret i64 %res
}

@fmt = private constant [12 x i8] c"%ld! = %ld\10\00"

define i32 @main(i32, i8**) {
  %n = add i64 15, 0
  %res = call i64 @fact(i64 %n)

  %fmt = getelementptr [12 x i8], [12 x i8]* @fmt, i32 0, i32 0
  call i32(i8*, ...) @printf(i8* %fmt, i64 %n, i64 %res)
  ret i32 0
}

declare i32 @printf(i8*, ...)


; ModuleID = 'share/test.ll'
source_filename = "share/test.ll"

@fmt = private constant [12 x i8] c"%ld! = %ld\10\00"

define private i64 @fact_helper(i64 %n, i64 %acc) {
entry:
  %0 = icmp sle i64 %n, 1
  br i1 %0, label %le1, label %gt1

le1:                                              ; preds = %entry
  ret i64 %acc

gt1:                                              ; preds = %entry
  %acc1 = mul i64 %acc, %n
  %n1 = sub i64 %n, 1
  %res = musttail call i64 @fact_helper(i64 %n1, i64 %acc1)
  ret i64 %res
}

define private i64 @fact(i64 %n) {
  %res = call i64 @fact_helper(i64 %n, i64 0)
  ret i64 %res
}

define i32 @main(i32 %0, i8** %1) {
  %n = add i64 3, 0
  %res = call i64 @fact(i64 %n)
  %fmt = getelementptr [12 x i8], [12 x i8]* @fmt, i32 0, i32 0
  %3 = call i32 (i8*, ...) @printf(i8* %fmt, i64 %n, i64 %res)
  ret i32 0
}

declare i32 @printf(i8*, ...)

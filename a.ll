; ModuleID = 'cscript_module'
source_filename = "cscript_module"

@global_exit_code = global i32 100

define i32 @add(i32 %a, i32 %b) {
entry:
  %b2 = alloca i32, align 4
  %a1 = alloca i32, align 4
  store i32 %a, ptr %a1, align 4
  store i32 %b, ptr %b2, align 4
  %loadtmp = load i32, ptr %a1, align 4
  %loadtmp3 = load i32, ptr %b2, align 4
  %addtmp = add i32 %loadtmp, %loadtmp3
  ret i32 %addtmp
}

define i32 @main() {
entry:
  %final_result = alloca i32, align 4
  %sum_result = alloca i32, align 4
  %start_value = alloca i32, align 4
  store i32 10, ptr %start_value, align 4
  %loadtmp = load i32, ptr %start_value, align 4
  %calltmp = call i32 @add(i32 %loadtmp, i32 42)
  store i32 %calltmp, ptr %sum_result, align 4
  %loadtmp1 = load i32, ptr @global_exit_code, align 4
  %loadtmp2 = load i32, ptr %sum_result, align 4
  %addtmp = add i32 %loadtmp1, %loadtmp2
  store i32 %addtmp, ptr %final_result, align 4
  %loadtmp3 = load i32, ptr %final_result, align 4
  %cmptmp = icmp sgt i32 %loadtmp3, 150
  br i1 %cmptmp, label %then, label %else

then:                                             ; preds = %entry
  %loadtmp4 = load i32, ptr %final_result, align 4
  ret i32 %loadtmp4

merge:                                            ; No predecessors!
  unreachable

else:                                             ; preds = %entry
  ret i32 -1
}

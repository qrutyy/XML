  $ dune exec ./../bin/XML_llvm.exe -- -o factorial.ll <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)

  $ cat factorial.ll
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %t_6 = alloca i64, align 8
    %calltmp = call i64 @fac(i64 4)
    store i64 %calltmp, ptr %t_6, align 4
    %t_61 = load i64, ptr %t_6, align 4
    call void @print_int(i64 %t_61)
    store i64 0, ptr %t_7, align 4
    %t_72 = load i64, ptr %t_7, align 4
    store i64 %t_72, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @fac(i64 %n) {
  entry:
    %t_4 = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %eqtmp = icmp eq i64 %n2, 0
    store i1 %eqtmp, ptr %t_0, align 1
    %t_03 = load i64, ptr %t_0, align 4
    %cond = icmp ne i64 %t_03, 0
    br i1 %cond, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %ifcont
  
  else:                                             ; preds = %entry
    %n4 = load i64, ptr %n1, align 4
    %subtmp = sub i64 %n4, 1
    store i64 %subtmp, ptr %t_1, align 4
    %t_15 = load i64, ptr %t_1, align 4
    %calltmp = call i64 @fac(i64 %t_15)
    store i64 %calltmp, ptr %t_2, align 4
    %n6 = load i64, ptr %n1, align 4
    %t_27 = load i64, ptr %t_2, align 4
    %multmp = mul i64 %n6, %t_27
    store i64 %multmp, ptr %t_3, align 4
    %t_38 = load i64, ptr %t_3, align 4
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ 1, %then ], [ %t_38, %else ]
    store i64 %iftmp, ptr %t_4, align 4
    %t_49 = load i64, ptr %t_4, align 4
    ret i64 %t_49
  }

====================== Fibonacci ======================
  $ ../bin/XML_llvm.exe -o fibonacci.s <<EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
  > 
  > let main = print_int (fib 6)

  $ cat fibonacci.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_9 = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %calltmp = call i64 @fib(i64 6)
    store i64 %calltmp, ptr %t_8, align 4
    %t_81 = load i64, ptr %t_8, align 4
    call void @print_int(i64 %t_81)
    store i64 0, ptr %t_9, align 4
    %t_92 = load i64, ptr %t_9, align 4
    store i64 %t_92, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @fib(i64 %n) {
  entry:
    %t_6 = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    %sletmp = icmp sle i64 %n2, 1
    store i1 %sletmp, ptr %t_0, align 1
    %t_03 = load i64, ptr %t_0, align 4
    %cond = icmp ne i64 %t_03, 0
    br i1 %cond, label %then, label %else
  
  then:                                             ; preds = %entry
    %n4 = load i64, ptr %n1, align 4
    br label %ifcont
  
  else:                                             ; preds = %entry
    %n5 = load i64, ptr %n1, align 4
    %subtmp = sub i64 %n5, 1
    store i64 %subtmp, ptr %t_1, align 4
    %t_16 = load i64, ptr %t_1, align 4
    %calltmp = call i64 @fib(i64 %t_16)
    store i64 %calltmp, ptr %t_2, align 4
    %n7 = load i64, ptr %n1, align 4
    %subtmp8 = sub i64 %n7, 2
    store i64 %subtmp8, ptr %t_3, align 4
    %t_39 = load i64, ptr %t_3, align 4
    %calltmp10 = call i64 @fib(i64 %t_39)
    store i64 %calltmp10, ptr %t_4, align 4
    %t_211 = load i64, ptr %t_2, align 4
    %t_412 = load i64, ptr %t_4, align 4
    %addtmp = add i64 %t_211, %t_412
    store i64 %addtmp, ptr %t_5, align 4
    %t_513 = load i64, ptr %t_5, align 4
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %n4, %then ], [ %t_513, %else ]
    store i64 %iftmp, ptr %t_6, align 4
    %t_614 = load i64, ptr %t_6, align 4
    ret i64 %t_614
  }

====================== Ififif ======================
  $ ../bin/XML_llvm.exe -o ififif.s <<EOF
  > let large x = if 0<>x then print_int 0 else print_int 1
  > let main =
  >   let x = if (if (if 0 = 1
  >                   then 0 = 1 else (let t42 = print_int 42 in 1 = 1))
  >               then 0 else 1) = 1
  >           then 0 else 1 in
  >   large x

  $ cat ififif.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_13 = alloca i64, align 8
    %x = alloca i64, align 8
    %t_12 = alloca i64, align 8
    %t_11 = alloca i64, align 8
    %t_10 = alloca i64, align 8
    %t_9 = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %t42 = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %t_6 = alloca i64, align 8
    %t_5 = alloca i64, align 8
    store i1 false, ptr %t_5, align 1
    %t_51 = load i64, ptr %t_5, align 4
    %cond = icmp ne i64 %t_51, 0
    br i1 %cond, label %then, label %else
  
  then:                                             ; preds = %entry
    store i1 false, ptr %t_6, align 1
    %t_62 = load i64, ptr %t_6, align 4
    br label %ifcont
  
  else:                                             ; preds = %entry
    call void @print_int(i64 42)
    store i64 0, ptr %t_7, align 4
    %t_73 = load i64, ptr %t_7, align 4
    store i64 %t_73, ptr %t42, align 4
    store i1 true, ptr %t_8, align 1
    %t_84 = load i64, ptr %t_8, align 4
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %t_62, %then ], [ %t_84, %else ]
    store i64 %iftmp, ptr %t_9, align 4
    %t_95 = load i64, ptr %t_9, align 4
    %cond6 = icmp ne i64 %t_95, 0
    br i1 %cond6, label %then7, label %else8
  
  then7:                                            ; preds = %ifcont
    br label %ifcont9
  
  else8:                                            ; preds = %ifcont
    br label %ifcont9
  
  ifcont9:                                          ; preds = %else8, %then7
    %iftmp10 = phi i64 [ 0, %then7 ], [ 1, %else8 ]
    store i64 %iftmp10, ptr %t_10, align 4
    %t_1011 = load i64, ptr %t_10, align 4
    %eqtmp = icmp eq i64 %t_1011, 1
    store i1 %eqtmp, ptr %t_11, align 1
    %t_1112 = load i64, ptr %t_11, align 4
    %cond13 = icmp ne i64 %t_1112, 0
    br i1 %cond13, label %then14, label %else15
  
  then14:                                           ; preds = %ifcont9
    br label %ifcont16
  
  else15:                                           ; preds = %ifcont9
    br label %ifcont16
  
  ifcont16:                                         ; preds = %else15, %then14
    %iftmp17 = phi i64 [ 0, %then14 ], [ 1, %else15 ]
    store i64 %iftmp17, ptr %t_12, align 4
    %t_1218 = load i64, ptr %t_12, align 4
    store i64 %t_1218, ptr %x, align 4
    %x19 = load i64, ptr %x, align 4
    %calltmp = call i64 @large(i64 %x19)
    store i64 %calltmp, ptr %t_13, align 4
    %t_1320 = load i64, ptr %t_13, align 4
    store i64 %t_1320, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @large(i64 %x) {
  entry:
    %t_3 = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %x1 = alloca i64, align 8
    store i64 %x, ptr %x1, align 4
    %x2 = load i64, ptr %x1, align 4
    %neqtmp = icmp ne i64 0, %x2
    store i1 %neqtmp, ptr %t_0, align 1
    %t_03 = load i64, ptr %t_0, align 4
    %cond = icmp ne i64 %t_03, 0
    br i1 %cond, label %then, label %else
  
  then:                                             ; preds = %entry
    call void @print_int(i64 0)
    store i64 0, ptr %t_1, align 4
    %t_14 = load i64, ptr %t_1, align 4
    br label %ifcont
  
  else:                                             ; preds = %entry
    call void @print_int(i64 1)
    store i64 0, ptr %t_2, align 4
    %t_25 = load i64, ptr %t_2, align 4
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %t_14, %then ], [ %t_25, %else ]
    store i64 %iftmp, ptr %t_3, align 4
    %t_36 = load i64, ptr %t_3, align 4
    ret i64 %t_36
  }

====================== Simple Closure ======================
  $ ../bin/XML_llvm.exe -o closure.s <<EOF
  > let simplesum x y = x + y
  > let partialapp_sum = simplesum 5
  > let main = print_int (partialapp_sum 5)
  $ cat closure.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %partialapp_sum = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @simplesum to i64), i64 2)
    %app_tmp = call i64 @apply1(i64 %closure_tmp, i64 5)
    store i64 %app_tmp, ptr %t_2, align 4
    %t_21 = load i64, ptr %t_2, align 4
    store i64 %t_21, ptr %partialapp_sum, align 4
    %clos_as_i64 = ptrtoint ptr %partialapp_sum to i64
    %app_tmp2 = call i64 @apply1(i64 %clos_as_i64, i64 5)
    store i64 %app_tmp2, ptr %t_3, align 4
    %t_33 = load i64, ptr %t_3, align 4
    call void @print_int(i64 %t_33)
    store i64 0, ptr %t_4, align 4
    %t_44 = load i64, ptr %t_4, align 4
    store i64 %t_44, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @simplesum(i64 %x, i64 %y) {
  entry:
    %t_0 = alloca i64, align 8
    %y2 = alloca i64, align 8
    %x1 = alloca i64, align 8
    store i64 %x, ptr %x1, align 4
    store i64 %y, ptr %y2, align 4
    %x3 = load i64, ptr %x1, align 4
    %y4 = load i64, ptr %y2, align 4
    %addtmp = add i64 %x3, %y4
    store i64 %addtmp, ptr %t_0, align 4
    %t_05 = load i64, ptr %t_0, align 4
    ret i64 %t_05
  }

====================== CPS Factorial ======================
  $ ../bin/XML_llvm.exe -fromfile manytests/typed/010faccps_ll.ml -o 010faccps_ll.s

  $ cat 010faccps_ll.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_15 = alloca i64, align 8
    %t_14 = alloca i64, align 8
    %t_13 = alloca i64, align 8
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %app_tmp = call i64 @apply1(i64 %closure_tmp, i64 4)
    store i64 %app_tmp, ptr %t_13, align 4
    %closure_tmp1 = call i64 @alloc_closure(i64 ptrtoint (ptr @id to i64), i64 1)
    %clos_as_i64 = ptrtoint ptr %t_13 to i64
    %app_tmp2 = call i64 @apply1(i64 %clos_as_i64, i64 %closure_tmp1)
    store i64 %app_tmp2, ptr %t_14, align 4
    %t_143 = load i64, ptr %t_14, align 4
    call void @print_int(i64 %t_143)
    store i64 0, ptr %t_15, align 4
    store i64 0, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @id(i64 %x) {
  entry:
    %x1 = alloca i64, align 8
    store i64 %x, ptr %x1, align 4
    %x2 = load i64, ptr %x1, align 4
    ret i64 %x2
  }
  
  define i64 @fresh_1(i64 %n, i64 %k, i64 %p) {
  entry:
    %t_2 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %p3 = alloca i64, align 8
    %k2 = alloca i64, align 8
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    store i64 %k, ptr %k2, align 4
    store i64 %p, ptr %p3, align 4
    %p4 = load i64, ptr %p3, align 4
    %n5 = load i64, ptr %n1, align 4
    %multmp = mul i64 %p4, %n5
    store i64 %multmp, ptr %t_1, align 4
    %t_16 = load i64, ptr %t_1, align 4
    %clos_as_i64 = ptrtoint ptr %k2 to i64
    %app_tmp = call i64 @apply1(i64 %clos_as_i64, i64 %t_16)
    store i64 %app_tmp, ptr %t_2, align 4
    %t_27 = load i64, ptr %t_2, align 4
    ret i64 %t_27
  }
  
  define i64 @fac_cps(i64 %n, i64 %k) {
  entry:
    %t_11 = alloca i64, align 8
    %t_10 = alloca i64, align 8
    %t_9 = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %t_6 = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %k2 = alloca i64, align 8
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    store i64 %k, ptr %k2, align 4
    %n3 = load i64, ptr %n1, align 4
    %eqtmp = icmp eq i64 %n3, 1
    store i1 %eqtmp, ptr %t_4, align 1
    %t_44 = load i64, ptr %t_4, align 4
    %cond = icmp ne i64 %t_44, 0
    br i1 %cond, label %then, label %else
  
  then:                                             ; preds = %entry
    %clos_as_i64 = ptrtoint ptr %k2 to i64
    %app_tmp = call i64 @apply1(i64 %clos_as_i64, i64 1)
    store i64 %app_tmp, ptr %t_5, align 4
    %t_55 = load i64, ptr %t_5, align 4
    br label %ifcont
  
  else:                                             ; preds = %entry
    %n6 = load i64, ptr %n1, align 4
    %subtmp = sub i64 %n6, 1
    store i64 %subtmp, ptr %t_6, align 4
    %t_67 = load i64, ptr %t_6, align 4
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %app_tmp8 = call i64 @apply1(i64 %closure_tmp, i64 %t_67)
    store i64 %app_tmp8, ptr %t_7, align 4
    %n9 = load i64, ptr %n1, align 4
    %closure_tmp10 = call i64 @alloc_closure(i64 ptrtoint (ptr @fresh_1 to i64), i64 3)
    %app_tmp11 = call i64 @apply1(i64 %closure_tmp10, i64 %n9)
    store i64 %app_tmp11, ptr %t_8, align 4
    %k12 = load i64, ptr %k2, align 4
    %clos_as_i6413 = ptrtoint ptr %t_8 to i64
    %app_tmp14 = call i64 @apply1(i64 %clos_as_i6413, i64 %k12)
    store i64 %app_tmp14, ptr %t_9, align 4
    %t_915 = load i64, ptr %t_9, align 4
    %clos_as_i6416 = ptrtoint ptr %t_7 to i64
    %app_tmp17 = call i64 @apply1(i64 %clos_as_i6416, i64 %t_915)
    store i64 %app_tmp17, ptr %t_10, align 4
    %t_1018 = load i64, ptr %t_10, align 4
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %t_55, %then ], [ %t_1018, %else ]
    store i64 %iftmp, ptr %t_11, align 4
    %t_1119 = load i64, ptr %t_11, align 4
    ret i64 %t_1119
  }

====================== CPS Fibbo ======================
  $ ../bin/XML_llvm.exe -fromfile manytests/typed/010fibcps_ll.ml -o 010fibcps_ll.s

  $ cat 010fibcps_ll.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %z = alloca i64, align 8
    %t_22 = alloca i64, align 8
    %t_21 = alloca i64, align 8
    %t_20 = alloca i64, align 8
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @fib to i64), i64 2)
    %app_tmp = call i64 @apply1(i64 %closure_tmp, i64 6)
    store i64 %app_tmp, ptr %t_20, align 4
    %closure_tmp1 = call i64 @alloc_closure(i64 ptrtoint (ptr @id to i64), i64 1)
    %clos_as_i64 = ptrtoint ptr %t_20 to i64
    %app_tmp2 = call i64 @apply1(i64 %clos_as_i64, i64 %closure_tmp1)
    store i64 %app_tmp2, ptr %t_21, align 4
    %t_213 = load i64, ptr %t_21, align 4
    call void @print_int(i64 %t_213)
    store i64 0, ptr %t_22, align 4
    %t_224 = load i64, ptr %t_22, align 4
    store i64 %t_224, ptr %z, align 4
    store i64 0, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @id(i64 %x) {
  entry:
    %x1 = alloca i64, align 8
    store i64 %x, ptr %x1, align 4
    %x2 = load i64, ptr %x1, align 4
    ret i64 %x2
  }
  
  define i64 @fresh_2(i64 %p1, i64 %k, i64 %p2) {
  entry:
    %t_2 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %p23 = alloca i64, align 8
    %k2 = alloca i64, align 8
    %p11 = alloca i64, align 8
    store i64 %p1, ptr %p11, align 4
    store i64 %k, ptr %k2, align 4
    store i64 %p2, ptr %p23, align 4
    %p14 = load i64, ptr %p11, align 4
    %p25 = load i64, ptr %p23, align 4
    %addtmp = add i64 %p14, %p25
    store i64 %addtmp, ptr %t_1, align 4
    %t_16 = load i64, ptr %t_1, align 4
    %clos_as_i64 = ptrtoint ptr %k2 to i64
    %app_tmp = call i64 @apply1(i64 %clos_as_i64, i64 %t_16)
    store i64 %app_tmp, ptr %t_2, align 4
    %t_27 = load i64, ptr %t_2, align 4
    ret i64 %t_27
  }
  
  define i64 @fresh_1(i64 %n, i64 %k, i64 %fib, i64 %p1) {
  entry:
    %t_8 = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %t_6 = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %p14 = alloca i64, align 8
    %fib3 = alloca i64, align 8
    %k2 = alloca i64, align 8
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    store i64 %k, ptr %k2, align 4
    store i64 %fib, ptr %fib3, align 4
    store i64 %p1, ptr %p14, align 4
    %n5 = load i64, ptr %n1, align 4
    %subtmp = sub i64 %n5, 2
    store i64 %subtmp, ptr %t_4, align 4
    %t_46 = load i64, ptr %t_4, align 4
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @fib to i64), i64 2)
    %app_tmp = call i64 @apply1(i64 %closure_tmp, i64 %t_46)
    store i64 %app_tmp, ptr %t_5, align 4
    %p17 = load i64, ptr %p14, align 4
    %closure_tmp8 = call i64 @alloc_closure(i64 ptrtoint (ptr @fresh_2 to i64), i64 3)
    %app_tmp9 = call i64 @apply1(i64 %closure_tmp8, i64 %p17)
    store i64 %app_tmp9, ptr %t_6, align 4
    %k10 = load i64, ptr %k2, align 4
    %clos_as_i64 = ptrtoint ptr %t_6 to i64
    %app_tmp11 = call i64 @apply1(i64 %clos_as_i64, i64 %k10)
    store i64 %app_tmp11, ptr %t_7, align 4
    %t_712 = load i64, ptr %t_7, align 4
    %clos_as_i6413 = ptrtoint ptr %t_5 to i64
    %app_tmp14 = call i64 @apply1(i64 %clos_as_i6413, i64 %t_712)
    store i64 %app_tmp14, ptr %t_8, align 4
    %t_815 = load i64, ptr %t_8, align 4
    ret i64 %t_815
  }
  
  define i64 @fib(i64 %n, i64 %k) {
  entry:
    %t_18 = alloca i64, align 8
    %t_17 = alloca i64, align 8
    %t_16 = alloca i64, align 8
    %t_15 = alloca i64, align 8
    %t_14 = alloca i64, align 8
    %t_13 = alloca i64, align 8
    %t_12 = alloca i64, align 8
    %t_11 = alloca i64, align 8
    %t_10 = alloca i64, align 8
    %k2 = alloca i64, align 8
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    store i64 %k, ptr %k2, align 4
    %n3 = load i64, ptr %n1, align 4
    %slttmp = icmp slt i64 %n3, 2
    store i1 %slttmp, ptr %t_10, align 1
    %t_104 = load i64, ptr %t_10, align 4
    %cond = icmp ne i64 %t_104, 0
    br i1 %cond, label %then, label %else
  
  then:                                             ; preds = %entry
    %n5 = load i64, ptr %n1, align 4
    %clos_as_i64 = ptrtoint ptr %k2 to i64
    %app_tmp = call i64 @apply1(i64 %clos_as_i64, i64 %n5)
    store i64 %app_tmp, ptr %t_11, align 4
    %t_116 = load i64, ptr %t_11, align 4
    br label %ifcont
  
  else:                                             ; preds = %entry
    %n7 = load i64, ptr %n1, align 4
    %subtmp = sub i64 %n7, 1
    store i64 %subtmp, ptr %t_12, align 4
    %t_128 = load i64, ptr %t_12, align 4
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @fib to i64), i64 2)
    %app_tmp9 = call i64 @apply1(i64 %closure_tmp, i64 %t_128)
    store i64 %app_tmp9, ptr %t_13, align 4
    %n10 = load i64, ptr %n1, align 4
    %closure_tmp11 = call i64 @alloc_closure(i64 ptrtoint (ptr @fresh_1 to i64), i64 4)
    %app_tmp12 = call i64 @apply1(i64 %closure_tmp11, i64 %n10)
    store i64 %app_tmp12, ptr %t_14, align 4
    %k13 = load i64, ptr %k2, align 4
    %clos_as_i6414 = ptrtoint ptr %t_14 to i64
    %app_tmp15 = call i64 @apply1(i64 %clos_as_i6414, i64 %k13)
    store i64 %app_tmp15, ptr %t_15, align 4
    %closure_tmp16 = call i64 @alloc_closure(i64 ptrtoint (ptr @fib to i64), i64 2)
    %clos_as_i6417 = ptrtoint ptr %t_15 to i64
    %app_tmp18 = call i64 @apply1(i64 %clos_as_i6417, i64 %closure_tmp16)
    store i64 %app_tmp18, ptr %t_16, align 4
    %t_1619 = load i64, ptr %t_16, align 4
    %clos_as_i6420 = ptrtoint ptr %t_13 to i64
    %app_tmp21 = call i64 @apply1(i64 %clos_as_i6420, i64 %t_1619)
    store i64 %app_tmp21, ptr %t_17, align 4
    %t_1722 = load i64, ptr %t_17, align 4
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %t_116, %then ], [ %t_1722, %else ]
    store i64 %iftmp, ptr %t_18, align 4
    %t_1823 = load i64, ptr %t_18, align 4
    ret i64 %t_1823
  }

  $ ../bin/XML_llvm.exe -fromfile manytests/typed/004manyargs.ml -o 004manyargs.s

  $ cat 004manyargs.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %temp2 = alloca i64, align 8
    %t_32 = alloca i64, align 8
    %t_31 = alloca i64, align 8
    %t_30 = alloca i64, align 8
    %t_29 = alloca i64, align 8
    %t_28 = alloca i64, align 8
    %rez = alloca i64, align 8
    %t_27 = alloca i64, align 8
    %t_26 = alloca i64, align 8
    %t_25 = alloca i64, align 8
    %t_24 = alloca i64, align 8
    %t_23 = alloca i64, align 8
    %t_22 = alloca i64, align 8
    %t_21 = alloca i64, align 8
    %t_20 = alloca i64, align 8
    %t_19 = alloca i64, align 8
    %t_18 = alloca i64, align 8
    %t_17 = alloca i64, align 8
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @test10 to i64), i64 10)
    %calltmp = call i64 @wrap(i64 %closure_tmp)
    store i64 %calltmp, ptr %t_17, align 4
    %clos_as_i64 = ptrtoint ptr %t_17 to i64
    %app_tmp = call i64 @apply1(i64 %clos_as_i64, i64 1)
    store i64 %app_tmp, ptr %t_18, align 4
    %clos_as_i641 = ptrtoint ptr %t_18 to i64
    %app_tmp2 = call i64 @apply1(i64 %clos_as_i641, i64 10)
    store i64 %app_tmp2, ptr %t_19, align 4
    %clos_as_i643 = ptrtoint ptr %t_19 to i64
    %app_tmp4 = call i64 @apply1(i64 %clos_as_i643, i64 100)
    store i64 %app_tmp4, ptr %t_20, align 4
    %clos_as_i645 = ptrtoint ptr %t_20 to i64
    %app_tmp6 = call i64 @apply1(i64 %clos_as_i645, i64 1000)
    store i64 %app_tmp6, ptr %t_21, align 4
    %clos_as_i647 = ptrtoint ptr %t_21 to i64
    %app_tmp8 = call i64 @apply1(i64 %clos_as_i647, i64 10000)
    store i64 %app_tmp8, ptr %t_22, align 4
    %clos_as_i649 = ptrtoint ptr %t_22 to i64
    %app_tmp10 = call i64 @apply1(i64 %clos_as_i649, i64 100000)
    store i64 %app_tmp10, ptr %t_23, align 4
    %clos_as_i6411 = ptrtoint ptr %t_23 to i64
    %app_tmp12 = call i64 @apply1(i64 %clos_as_i6411, i64 1000000)
    store i64 %app_tmp12, ptr %t_24, align 4
    %clos_as_i6413 = ptrtoint ptr %t_24 to i64
    %app_tmp14 = call i64 @apply1(i64 %clos_as_i6413, i64 10000000)
    store i64 %app_tmp14, ptr %t_25, align 4
    %clos_as_i6415 = ptrtoint ptr %t_25 to i64
    %app_tmp16 = call i64 @apply1(i64 %clos_as_i6415, i64 100000000)
    store i64 %app_tmp16, ptr %t_26, align 4
    %clos_as_i6417 = ptrtoint ptr %t_26 to i64
    %app_tmp18 = call i64 @apply1(i64 %clos_as_i6417, i64 1000000000)
    store i64 %app_tmp18, ptr %t_27, align 4
    %t_2719 = load i64, ptr %t_27, align 4
    store i64 %t_2719, ptr %rez, align 4
    %rez20 = load i64, ptr %rez, align 4
    call void @print_int(i64 %rez20)
    store i64 0, ptr %t_28, align 4
    %closure_tmp21 = call i64 @alloc_closure(i64 ptrtoint (ptr @test3 to i64), i64 3)
    %calltmp22 = call i64 @wrap(i64 %closure_tmp21)
    store i64 %calltmp22, ptr %t_29, align 4
    %clos_as_i6423 = ptrtoint ptr %t_29 to i64
    %app_tmp24 = call i64 @apply1(i64 %clos_as_i6423, i64 1)
    store i64 %app_tmp24, ptr %t_30, align 4
    %clos_as_i6425 = ptrtoint ptr %t_30 to i64
    %app_tmp26 = call i64 @apply1(i64 %clos_as_i6425, i64 10)
    store i64 %app_tmp26, ptr %t_31, align 4
    %clos_as_i6427 = ptrtoint ptr %t_31 to i64
    %app_tmp28 = call i64 @apply1(i64 %clos_as_i6427, i64 100)
    store i64 %app_tmp28, ptr %t_32, align 4
    %t_3229 = load i64, ptr %t_32, align 4
    store i64 %t_3229, ptr %temp2, align 4
    store i64 0, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @wrap(i64 %f) {
  entry:
    %t_1 = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %f1 = alloca i64, align 8
    store i64 %f, ptr %f1, align 4
    store i1 true, ptr %t_0, align 1
    %t_02 = load i64, ptr %t_0, align 4
    %cond = icmp ne i64 %t_02, 0
    br i1 %cond, label %then, label %else
  
  then:                                             ; preds = %entry
    %f3 = load i64, ptr %f1, align 4
    br label %ifcont
  
  else:                                             ; preds = %entry
    %f4 = load i64, ptr %f1, align 4
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %f3, %then ], [ %f4, %else ]
    store i64 %iftmp, ptr %t_1, align 4
    %t_15 = load i64, ptr %t_1, align 4
    ret i64 %t_15
  }
  
  define i64 @test3(i64 %a, i64 %b, i64 %c) {
  entry:
    %c12 = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %b9 = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %a6 = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %c3 = alloca i64, align 8
    %b2 = alloca i64, align 8
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    store i64 %b, ptr %b2, align 4
    store i64 %c, ptr %c3, align 4
    %a4 = load i64, ptr %a1, align 4
    call void @print_int(i64 %a4)
    store i64 0, ptr %t_3, align 4
    %t_35 = load i64, ptr %t_3, align 4
    store i64 %t_35, ptr %a6, align 4
    %b7 = load i64, ptr %b2, align 4
    call void @print_int(i64 %b7)
    store i64 0, ptr %t_4, align 4
    %t_48 = load i64, ptr %t_4, align 4
    store i64 %t_48, ptr %b9, align 4
    %c10 = load i64, ptr %c3, align 4
    call void @print_int(i64 %c10)
    store i64 0, ptr %t_5, align 4
    %t_511 = load i64, ptr %t_5, align 4
    store i64 %t_511, ptr %c12, align 4
    ret i64 0
  }
  
  define i64 @test10(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f, i64 %g, i64 %h, i64 %i, i64 %j) {
  entry:
    %t_15 = alloca i64, align 8
    %t_14 = alloca i64, align 8
    %t_13 = alloca i64, align 8
    %t_12 = alloca i64, align 8
    %t_11 = alloca i64, align 8
    %t_10 = alloca i64, align 8
    %t_9 = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %j10 = alloca i64, align 8
    %i9 = alloca i64, align 8
    %h8 = alloca i64, align 8
    %g7 = alloca i64, align 8
    %f6 = alloca i64, align 8
    %e5 = alloca i64, align 8
    %d4 = alloca i64, align 8
    %c3 = alloca i64, align 8
    %b2 = alloca i64, align 8
    %a1 = alloca i64, align 8
    store i64 %a, ptr %a1, align 4
    store i64 %b, ptr %b2, align 4
    store i64 %c, ptr %c3, align 4
    store i64 %d, ptr %d4, align 4
    store i64 %e, ptr %e5, align 4
    store i64 %f, ptr %f6, align 4
    store i64 %g, ptr %g7, align 4
    store i64 %h, ptr %h8, align 4
    store i64 %i, ptr %i9, align 4
    store i64 %j, ptr %j10, align 4
    %a11 = load i64, ptr %a1, align 4
    %b12 = load i64, ptr %b2, align 4
    %addtmp = add i64 %a11, %b12
    store i64 %addtmp, ptr %t_7, align 4
    %t_713 = load i64, ptr %t_7, align 4
    %c14 = load i64, ptr %c3, align 4
    %addtmp15 = add i64 %t_713, %c14
    store i64 %addtmp15, ptr %t_8, align 4
    %t_816 = load i64, ptr %t_8, align 4
    %d17 = load i64, ptr %d4, align 4
    %addtmp18 = add i64 %t_816, %d17
    store i64 %addtmp18, ptr %t_9, align 4
    %t_919 = load i64, ptr %t_9, align 4
    %e20 = load i64, ptr %e5, align 4
    %addtmp21 = add i64 %t_919, %e20
    store i64 %addtmp21, ptr %t_10, align 4
    %t_1022 = load i64, ptr %t_10, align 4
    %f23 = load i64, ptr %f6, align 4
    %addtmp24 = add i64 %t_1022, %f23
    store i64 %addtmp24, ptr %t_11, align 4
    %t_1125 = load i64, ptr %t_11, align 4
    %g26 = load i64, ptr %g7, align 4
    %addtmp27 = add i64 %t_1125, %g26
    store i64 %addtmp27, ptr %t_12, align 4
    %t_1228 = load i64, ptr %t_12, align 4
    %h29 = load i64, ptr %h8, align 4
    %addtmp30 = add i64 %t_1228, %h29
    store i64 %addtmp30, ptr %t_13, align 4
    %t_1331 = load i64, ptr %t_13, align 4
    %i32 = load i64, ptr %i9, align 4
    %addtmp33 = add i64 %t_1331, %i32
    store i64 %addtmp33, ptr %t_14, align 4
    %t_1434 = load i64, ptr %t_14, align 4
    %j35 = load i64, ptr %j10, align 4
    %addtmp36 = add i64 %t_1434, %j35
    store i64 %addtmp36, ptr %t_15, align 4
    %t_1537 = load i64, ptr %t_15, align 4
    ret i64 %t_1537
  }

  $ ../bin/XML_llvm.exe -o tuple_return.s <<EOF
  > let make_pair x y = (x, y)
  > 
  > let main =
  >   let p = make_pair 10 20 in
  >   let (a, b) = p in
  >   print_int (a + b)

  $ cat tuple_return.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_6 = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %b = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %a = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %p = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @make_pair to i64), i64 2)
    %app_tmp = call i64 @apply1(i64 %closure_tmp, i64 10)
    store i64 %app_tmp, ptr %t_2, align 4
    %clos_as_i64 = ptrtoint ptr %t_2 to i64
    %app_tmp1 = call i64 @apply1(i64 %clos_as_i64, i64 20)
    store i64 %app_tmp1, ptr %t_3, align 4
    %t_32 = load i64, ptr %t_3, align 4
    store i64 %t_32, ptr %p, align 4
    %p3 = load i64, ptr %p, align 4
    store i64 %p3, ptr %t_4, align 4
    %t_44 = load i64, ptr %t_4, align 4
    %load_tmp = call i64 @field(i64 %t_44, i64 0)
    store i64 %load_tmp, ptr %t_8, align 4
    %t_85 = load i64, ptr %t_8, align 4
    store i64 %t_85, ptr %a, align 4
    %t_46 = load i64, ptr %t_4, align 4
    %load_tmp7 = call i64 @field(i64 %t_46, i64 8)
    store i64 %load_tmp7, ptr %t_7, align 4
    %t_78 = load i64, ptr %t_7, align 4
    store i64 %t_78, ptr %b, align 4
    %a9 = load i64, ptr %a, align 4
    %b10 = load i64, ptr %b, align 4
    %addtmp = add i64 %a9, %b10
    store i64 %addtmp, ptr %t_5, align 4
    %t_511 = load i64, ptr %t_5, align 4
    call void @print_int(i64 %t_511)
    store i64 0, ptr %t_6, align 4
    %t_612 = load i64, ptr %t_6, align 4
    store i64 %t_612, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @make_pair(i64 %x, i64 %y) {
  entry:
    %t_0 = alloca i64, align 8
    %y2 = alloca i64, align 8
    %x1 = alloca i64, align 8
    store i64 %x, ptr %x1, align 4
    store i64 %y, ptr %y2, align 4
    %x3 = load i64, ptr %x1, align 4
    %y4 = load i64, ptr %y2, align 4
    %tuple_vals_alloca = alloca i64, i64 2, align 8
    %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
    store i64 %x3, ptr %ptr_to_elem, align 4
    %ptr_to_elem5 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
    store i64 %y4, ptr %ptr_to_elem5, align 4
    %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
    %tuple_tmp = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i64)
    store i64 %tuple_tmp, ptr %t_0, align 4
    %t_06 = load i64, ptr %t_0, align 4
    ret i64 %t_06
  }
 
  $ ../bin/XML_llvm.exe -o tuple_swap.s <<EOF
  > let swap p =
  >   let (a, b) = p in
  >   (b, a)
  > 
  > let main =
  >   let p1 = (1, 2) in
  >   let p2 = swap p1 in
  >   let (x, y) = p2 in
  >   print_int x

  $ cat tuple_swap.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %y = alloca i64, align 8
    %t_9 = alloca i64, align 8
    %x = alloca i64, align 8
    %t_10 = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %p2 = alloca i64, align 8
    %t_6 = alloca i64, align 8
    %p1 = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %tuple_vals_alloca = alloca i64, i64 2, align 8
    %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
    store i64 1, ptr %ptr_to_elem, align 4
    %ptr_to_elem1 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
    store i64 2, ptr %ptr_to_elem1, align 4
    %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
    %tuple_tmp = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i64)
    store i64 %tuple_tmp, ptr %t_5, align 4
    %t_52 = load i64, ptr %t_5, align 4
    store i64 %t_52, ptr %p1, align 4
    %p13 = load i64, ptr %p1, align 4
    %calltmp = call i64 @swap(i64 %p13)
    store i64 %calltmp, ptr %t_6, align 4
    %t_64 = load i64, ptr %t_6, align 4
    store i64 %t_64, ptr %p2, align 4
    %p25 = load i64, ptr %p2, align 4
    store i64 %p25, ptr %t_7, align 4
    %t_76 = load i64, ptr %t_7, align 4
    %load_tmp = call i64 @field(i64 %t_76, i64 0)
    store i64 %load_tmp, ptr %t_10, align 4
    %t_107 = load i64, ptr %t_10, align 4
    store i64 %t_107, ptr %x, align 4
    %t_78 = load i64, ptr %t_7, align 4
    %load_tmp9 = call i64 @field(i64 %t_78, i64 8)
    store i64 %load_tmp9, ptr %t_9, align 4
    %t_910 = load i64, ptr %t_9, align 4
    store i64 %t_910, ptr %y, align 4
    %x11 = load i64, ptr %x, align 4
    call void @print_int(i64 %x11)
    store i64 0, ptr %t_8, align 4
    %t_812 = load i64, ptr %t_8, align 4
    store i64 %t_812, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @swap(i64 %p) {
  entry:
    %t_1 = alloca i64, align 8
    %b = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %a = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %p1 = alloca i64, align 8
    store i64 %p, ptr %p1, align 4
    %p2 = load i64, ptr %p1, align 4
    store i64 %p2, ptr %t_0, align 4
    %t_03 = load i64, ptr %t_0, align 4
    %load_tmp = call i64 @field(i64 %t_03, i64 0)
    store i64 %load_tmp, ptr %t_3, align 4
    %t_34 = load i64, ptr %t_3, align 4
    store i64 %t_34, ptr %a, align 4
    %t_05 = load i64, ptr %t_0, align 4
    %load_tmp6 = call i64 @field(i64 %t_05, i64 8)
    store i64 %load_tmp6, ptr %t_2, align 4
    %t_27 = load i64, ptr %t_2, align 4
    store i64 %t_27, ptr %b, align 4
    %b8 = load i64, ptr %b, align 4
    %a9 = load i64, ptr %a, align 4
    %tuple_vals_alloca = alloca i64, i64 2, align 8
    %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
    store i64 %b8, ptr %ptr_to_elem, align 4
    %ptr_to_elem10 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
    store i64 %a9, ptr %ptr_to_elem10, align 4
    %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
    %tuple_tmp = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i64)
    store i64 %tuple_tmp, ptr %t_1, align 4
    %t_111 = load i64, ptr %t_1, align 4
    ret i64 %t_111
  }

  $ ../bin/XML_llvm.exe -o tuple_order.s <<EOF
  > let f n =
  >   n
  > 
  > let main =
  >   let t = (f 10, f 20) in
  >   let (a, b) = t in
  >   print_int (a + b)

  $ cat tuple_order.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_6 = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %b = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %a = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %t = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %calltmp = call i64 @f(i64 10)
    store i64 %calltmp, ptr %t_1, align 4
    %calltmp1 = call i64 @f(i64 20)
    store i64 %calltmp1, ptr %t_2, align 4
    %t_12 = load i64, ptr %t_1, align 4
    %t_23 = load i64, ptr %t_2, align 4
    %tuple_vals_alloca = alloca i64, i64 2, align 8
    %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
    store i64 %t_12, ptr %ptr_to_elem, align 4
    %ptr_to_elem4 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
    store i64 %t_23, ptr %ptr_to_elem4, align 4
    %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
    %tuple_tmp = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i64)
    store i64 %tuple_tmp, ptr %t_3, align 4
    %t_35 = load i64, ptr %t_3, align 4
    store i64 %t_35, ptr %t, align 4
    %t6 = load i64, ptr %t, align 4
    store i64 %t6, ptr %t_4, align 4
    %t_47 = load i64, ptr %t_4, align 4
    %load_tmp = call i64 @field(i64 %t_47, i64 0)
    store i64 %load_tmp, ptr %t_8, align 4
    %t_88 = load i64, ptr %t_8, align 4
    store i64 %t_88, ptr %a, align 4
    %t_49 = load i64, ptr %t_4, align 4
    %load_tmp10 = call i64 @field(i64 %t_49, i64 8)
    store i64 %load_tmp10, ptr %t_7, align 4
    %t_711 = load i64, ptr %t_7, align 4
    store i64 %t_711, ptr %b, align 4
    %a12 = load i64, ptr %a, align 4
    %b13 = load i64, ptr %b, align 4
    %addtmp = add i64 %a12, %b13
    store i64 %addtmp, ptr %t_5, align 4
    %t_514 = load i64, ptr %t_5, align 4
    call void @print_int(i64 %t_514)
    store i64 0, ptr %t_6, align 4
    %t_615 = load i64, ptr %t_6, align 4
    store i64 %t_615, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @f(i64 %n) {
  entry:
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    %n2 = load i64, ptr %n1, align 4
    ret i64 %n2
  }

  $ ../bin/XML_llvm.exe -o tuple_linked_list.s <<EOF
  > let rec sum_list lst =
  >   if lst = 0 then 0 else
  >   let (head, tail) = lst in
  >   head + sum_list tail
  > 
  > let main =
  >   let lst = (10, (20, (30, 0))) in
  >   print_int (sum_list lst)

  $ cat tuple_linked_list.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_12 = alloca i64, align 8
    %t_11 = alloca i64, align 8
    %lst = alloca i64, align 8
    %t_10 = alloca i64, align 8
    %t_9 = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %tuple_vals_alloca = alloca i64, i64 2, align 8
    %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
    store i64 30, ptr %ptr_to_elem, align 4
    %ptr_to_elem1 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
    store i64 0, ptr %ptr_to_elem1, align 4
    %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
    %tuple_tmp = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i64)
    store i64 %tuple_tmp, ptr %t_8, align 4
    %t_82 = load i64, ptr %t_8, align 4
    %tuple_vals_alloca3 = alloca i64, i64 2, align 8
    %ptr_to_elem4 = getelementptr i64, ptr %tuple_vals_alloca3, i64 0
    store i64 20, ptr %ptr_to_elem4, align 4
    %ptr_to_elem5 = getelementptr i64, ptr %tuple_vals_alloca3, i64 1
    store i64 %t_82, ptr %ptr_to_elem5, align 4
    %alloca_as_i646 = ptrtoint ptr %tuple_vals_alloca3 to i64
    %tuple_tmp7 = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i646)
    store i64 %tuple_tmp7, ptr %t_9, align 4
    %t_98 = load i64, ptr %t_9, align 4
    %tuple_vals_alloca9 = alloca i64, i64 2, align 8
    %ptr_to_elem10 = getelementptr i64, ptr %tuple_vals_alloca9, i64 0
    store i64 10, ptr %ptr_to_elem10, align 4
    %ptr_to_elem11 = getelementptr i64, ptr %tuple_vals_alloca9, i64 1
    store i64 %t_98, ptr %ptr_to_elem11, align 4
    %alloca_as_i6412 = ptrtoint ptr %tuple_vals_alloca9 to i64
    %tuple_tmp13 = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i6412)
    store i64 %tuple_tmp13, ptr %t_10, align 4
    %t_1014 = load i64, ptr %t_10, align 4
    store i64 %t_1014, ptr %lst, align 4
    %lst15 = load i64, ptr %lst, align 4
    %calltmp = call i64 @sum_list(i64 %lst15)
    store i64 %calltmp, ptr %t_11, align 4
    %t_1116 = load i64, ptr %t_11, align 4
    call void @print_int(i64 %t_1116)
    store i64 0, ptr %t_12, align 4
    %t_1217 = load i64, ptr %t_12, align 4
    store i64 %t_1217, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @sum_list(i64 %lst) {
  entry:
    %t_6 = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %tail = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %head = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %lst1 = alloca i64, align 8
    store i64 %lst, ptr %lst1, align 4
    %lst2 = load i64, ptr %lst1, align 4
    %eqtmp = icmp eq i64 %lst2, 0
    store i1 %eqtmp, ptr %t_0, align 1
    %t_03 = load i64, ptr %t_0, align 4
    %cond = icmp ne i64 %t_03, 0
    br i1 %cond, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %ifcont
  
  else:                                             ; preds = %entry
    %lst4 = load i64, ptr %lst1, align 4
    store i64 %lst4, ptr %t_1, align 4
    %t_15 = load i64, ptr %t_1, align 4
    %load_tmp = call i64 @field(i64 %t_15, i64 0)
    store i64 %load_tmp, ptr %t_5, align 4
    %t_56 = load i64, ptr %t_5, align 4
    store i64 %t_56, ptr %head, align 4
    %t_17 = load i64, ptr %t_1, align 4
    %load_tmp8 = call i64 @field(i64 %t_17, i64 8)
    store i64 %load_tmp8, ptr %t_4, align 4
    %t_49 = load i64, ptr %t_4, align 4
    store i64 %t_49, ptr %tail, align 4
    %tail10 = load i64, ptr %tail, align 4
    %calltmp = call i64 @sum_list(i64 %tail10)
    store i64 %calltmp, ptr %t_2, align 4
    %head11 = load i64, ptr %head, align 4
    %t_212 = load i64, ptr %t_2, align 4
    %addtmp = add i64 %head11, %t_212
    store i64 %addtmp, ptr %t_3, align 4
    %t_313 = load i64, ptr %t_3, align 4
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ 0, %then ], [ %t_313, %else ]
    store i64 %iftmp, ptr %t_6, align 4
    %t_614 = load i64, ptr %t_6, align 4
    ret i64 %t_614
  }

  $ ../bin/XML_llvm.exe -o tuple_large.s <<EOF
  > let main =
  >   let t = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) in
  >   let (a, b, c, d, e, f, g, h, i, j) = t in
  >   print_int j

  $ cat tuple_large.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %j = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %i = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %h = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %g = alloca i64, align 8
    %t_6 = alloca i64, align 8
    %f = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %e = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %d = alloca i64, align 8
    %t_9 = alloca i64, align 8
    %c = alloca i64, align 8
    %t_10 = alloca i64, align 8
    %b = alloca i64, align 8
    %t_11 = alloca i64, align 8
    %a = alloca i64, align 8
    %t_12 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %t = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %tuple_vals_alloca = alloca i64, i64 10, align 8
    %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
    store i64 1, ptr %ptr_to_elem, align 4
    %ptr_to_elem1 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
    store i64 2, ptr %ptr_to_elem1, align 4
    %ptr_to_elem2 = getelementptr i64, ptr %tuple_vals_alloca, i64 2
    store i64 3, ptr %ptr_to_elem2, align 4
    %ptr_to_elem3 = getelementptr i64, ptr %tuple_vals_alloca, i64 3
    store i64 4, ptr %ptr_to_elem3, align 4
    %ptr_to_elem4 = getelementptr i64, ptr %tuple_vals_alloca, i64 4
    store i64 5, ptr %ptr_to_elem4, align 4
    %ptr_to_elem5 = getelementptr i64, ptr %tuple_vals_alloca, i64 5
    store i64 6, ptr %ptr_to_elem5, align 4
    %ptr_to_elem6 = getelementptr i64, ptr %tuple_vals_alloca, i64 6
    store i64 7, ptr %ptr_to_elem6, align 4
    %ptr_to_elem7 = getelementptr i64, ptr %tuple_vals_alloca, i64 7
    store i64 8, ptr %ptr_to_elem7, align 4
    %ptr_to_elem8 = getelementptr i64, ptr %tuple_vals_alloca, i64 8
    store i64 9, ptr %ptr_to_elem8, align 4
    %ptr_to_elem9 = getelementptr i64, ptr %tuple_vals_alloca, i64 9
    store i64 10, ptr %ptr_to_elem9, align 4
    %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
    %tuple_tmp = call i64 @create_tuple_init(i64 10, i64 %alloca_as_i64)
    store i64 %tuple_tmp, ptr %t_0, align 4
    %t_010 = load i64, ptr %t_0, align 4
    store i64 %t_010, ptr %t, align 4
    %t11 = load i64, ptr %t, align 4
    store i64 %t11, ptr %t_1, align 4
    %t_112 = load i64, ptr %t_1, align 4
    %load_tmp = call i64 @field(i64 %t_112, i64 0)
    store i64 %load_tmp, ptr %t_12, align 4
    %t_1213 = load i64, ptr %t_12, align 4
    store i64 %t_1213, ptr %a, align 4
    %t_114 = load i64, ptr %t_1, align 4
    %load_tmp15 = call i64 @field(i64 %t_114, i64 8)
    store i64 %load_tmp15, ptr %t_11, align 4
    %t_1116 = load i64, ptr %t_11, align 4
    store i64 %t_1116, ptr %b, align 4
    %t_117 = load i64, ptr %t_1, align 4
    %load_tmp18 = call i64 @field(i64 %t_117, i64 16)
    store i64 %load_tmp18, ptr %t_10, align 4
    %t_1019 = load i64, ptr %t_10, align 4
    store i64 %t_1019, ptr %c, align 4
    %t_120 = load i64, ptr %t_1, align 4
    %load_tmp21 = call i64 @field(i64 %t_120, i64 24)
    store i64 %load_tmp21, ptr %t_9, align 4
    %t_922 = load i64, ptr %t_9, align 4
    store i64 %t_922, ptr %d, align 4
    %t_123 = load i64, ptr %t_1, align 4
    %load_tmp24 = call i64 @field(i64 %t_123, i64 32)
    store i64 %load_tmp24, ptr %t_8, align 4
    %t_825 = load i64, ptr %t_8, align 4
    store i64 %t_825, ptr %e, align 4
    %t_126 = load i64, ptr %t_1, align 4
    %load_tmp27 = call i64 @field(i64 %t_126, i64 40)
    store i64 %load_tmp27, ptr %t_7, align 4
    %t_728 = load i64, ptr %t_7, align 4
    store i64 %t_728, ptr %f, align 4
    %t_129 = load i64, ptr %t_1, align 4
    %load_tmp30 = call i64 @field(i64 %t_129, i64 48)
    store i64 %load_tmp30, ptr %t_6, align 4
    %t_631 = load i64, ptr %t_6, align 4
    store i64 %t_631, ptr %g, align 4
    %t_132 = load i64, ptr %t_1, align 4
    %load_tmp33 = call i64 @field(i64 %t_132, i64 56)
    store i64 %load_tmp33, ptr %t_5, align 4
    %t_534 = load i64, ptr %t_5, align 4
    store i64 %t_534, ptr %h, align 4
    %t_135 = load i64, ptr %t_1, align 4
    %load_tmp36 = call i64 @field(i64 %t_135, i64 64)
    store i64 %load_tmp36, ptr %t_4, align 4
    %t_437 = load i64, ptr %t_4, align 4
    store i64 %t_437, ptr %i, align 4
    %t_138 = load i64, ptr %t_1, align 4
    %load_tmp39 = call i64 @field(i64 %t_138, i64 72)
    store i64 %load_tmp39, ptr %t_3, align 4
    %t_340 = load i64, ptr %t_3, align 4
    store i64 %t_340, ptr %j, align 4
    %j41 = load i64, ptr %j, align 4
    call void @print_int(i64 %j41)
    store i64 0, ptr %t_2, align 4
    %t_242 = load i64, ptr %t_2, align 4
    store i64 %t_242, ptr %main, align 4
    ret i64 0
  }

  $ ../bin/XML_llvm.exe -o tuple_basic.s <<EOF
  > let main =
  >   let t = (10, 20) in
  >   let (a, b) = t in
  >   print_int (a + b)

  $ cat tuple_basic.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %b = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %a = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %t = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %tuple_vals_alloca = alloca i64, i64 2, align 8
    %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
    store i64 10, ptr %ptr_to_elem, align 4
    %ptr_to_elem1 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
    store i64 20, ptr %ptr_to_elem1, align 4
    %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
    %tuple_tmp = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i64)
    store i64 %tuple_tmp, ptr %t_0, align 4
    %t_02 = load i64, ptr %t_0, align 4
    store i64 %t_02, ptr %t, align 4
    %t3 = load i64, ptr %t, align 4
    store i64 %t3, ptr %t_1, align 4
    %t_14 = load i64, ptr %t_1, align 4
    %load_tmp = call i64 @field(i64 %t_14, i64 0)
    store i64 %load_tmp, ptr %t_5, align 4
    %t_55 = load i64, ptr %t_5, align 4
    store i64 %t_55, ptr %a, align 4
    %t_16 = load i64, ptr %t_1, align 4
    %load_tmp7 = call i64 @field(i64 %t_16, i64 8)
    store i64 %load_tmp7, ptr %t_4, align 4
    %t_48 = load i64, ptr %t_4, align 4
    store i64 %t_48, ptr %b, align 4
    %a9 = load i64, ptr %a, align 4
    %b10 = load i64, ptr %b, align 4
    %addtmp = add i64 %a9, %b10
    store i64 %addtmp, ptr %t_2, align 4
    %t_211 = load i64, ptr %t_2, align 4
    call void @print_int(i64 %t_211)
    store i64 0, ptr %t_3, align 4
    %t_312 = load i64, ptr %t_3, align 4
    store i64 %t_312, ptr %main, align 4
    ret i64 0
  }

  $ ../bin/XML_llvm.exe -o tuple_nested.s <<EOF
  > let main =
  >   let complex = (100, (20, 3)) in
  >   let (a, (b, c)) = complex in
  >   print_int (a + b + c)

  $ cat tuple_nested.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %c = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %b = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %t_6 = alloca i64, align 8
    %a = alloca i64, align 8
    %t_9 = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %complex = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %tuple_vals_alloca = alloca i64, i64 2, align 8
    %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
    store i64 20, ptr %ptr_to_elem, align 4
    %ptr_to_elem1 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
    store i64 3, ptr %ptr_to_elem1, align 4
    %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
    %tuple_tmp = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i64)
    store i64 %tuple_tmp, ptr %t_0, align 4
    %t_02 = load i64, ptr %t_0, align 4
    %tuple_vals_alloca3 = alloca i64, i64 2, align 8
    %ptr_to_elem4 = getelementptr i64, ptr %tuple_vals_alloca3, i64 0
    store i64 100, ptr %ptr_to_elem4, align 4
    %ptr_to_elem5 = getelementptr i64, ptr %tuple_vals_alloca3, i64 1
    store i64 %t_02, ptr %ptr_to_elem5, align 4
    %alloca_as_i646 = ptrtoint ptr %tuple_vals_alloca3 to i64
    %tuple_tmp7 = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i646)
    store i64 %tuple_tmp7, ptr %t_1, align 4
    %t_18 = load i64, ptr %t_1, align 4
    store i64 %t_18, ptr %complex, align 4
    %complex9 = load i64, ptr %complex, align 4
    store i64 %complex9, ptr %t_2, align 4
    %t_210 = load i64, ptr %t_2, align 4
    %load_tmp = call i64 @field(i64 %t_210, i64 0)
    store i64 %load_tmp, ptr %t_9, align 4
    %t_911 = load i64, ptr %t_9, align 4
    store i64 %t_911, ptr %a, align 4
    %t_212 = load i64, ptr %t_2, align 4
    %load_tmp13 = call i64 @field(i64 %t_212, i64 8)
    store i64 %load_tmp13, ptr %t_6, align 4
    %t_614 = load i64, ptr %t_6, align 4
    %load_tmp15 = call i64 @field(i64 %t_614, i64 0)
    store i64 %load_tmp15, ptr %t_8, align 4
    %t_816 = load i64, ptr %t_8, align 4
    store i64 %t_816, ptr %b, align 4
    %t_617 = load i64, ptr %t_6, align 4
    %load_tmp18 = call i64 @field(i64 %t_617, i64 8)
    store i64 %load_tmp18, ptr %t_7, align 4
    %t_719 = load i64, ptr %t_7, align 4
    store i64 %t_719, ptr %c, align 4
    %a20 = load i64, ptr %a, align 4
    %b21 = load i64, ptr %b, align 4
    %addtmp = add i64 %a20, %b21
    store i64 %addtmp, ptr %t_3, align 4
    %t_322 = load i64, ptr %t_3, align 4
    %c23 = load i64, ptr %c, align 4
    %addtmp24 = add i64 %t_322, %c23
    store i64 %addtmp24, ptr %t_4, align 4
    %t_425 = load i64, ptr %t_4, align 4
    call void @print_int(i64 %t_425)
    store i64 0, ptr %t_5, align 4
    %t_526 = load i64, ptr %t_5, align 4
    store i64 %t_526, ptr %main, align 4
    ret i64 0
  }

  $ ../bin/XML_llvm.exe -o tuple_arg.s <<EOF
  > let sum_pair p =
  >   let (x, y) = p in
  >   x + y
  > 
  > let main =
  >   let p = (40, 2) in
  >   print_int (sum_pair p)

  $ cat tuple_arg.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %t_6 = alloca i64, align 8
    %p = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %tuple_vals_alloca = alloca i64, i64 2, align 8
    %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
    store i64 40, ptr %ptr_to_elem, align 4
    %ptr_to_elem1 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
    store i64 2, ptr %ptr_to_elem1, align 4
    %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
    %tuple_tmp = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i64)
    store i64 %tuple_tmp, ptr %t_5, align 4
    %t_52 = load i64, ptr %t_5, align 4
    store i64 %t_52, ptr %p, align 4
    %p3 = load i64, ptr %p, align 4
    %calltmp = call i64 @sum_pair(i64 %p3)
    store i64 %calltmp, ptr %t_6, align 4
    %t_64 = load i64, ptr %t_6, align 4
    call void @print_int(i64 %t_64)
    store i64 0, ptr %t_7, align 4
    %t_75 = load i64, ptr %t_7, align 4
    store i64 %t_75, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @sum_pair(i64 %p) {
  entry:
    %t_1 = alloca i64, align 8
    %y = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %x = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %p1 = alloca i64, align 8
    store i64 %p, ptr %p1, align 4
    %p2 = load i64, ptr %p1, align 4
    store i64 %p2, ptr %t_0, align 4
    %t_03 = load i64, ptr %t_0, align 4
    %load_tmp = call i64 @field(i64 %t_03, i64 0)
    store i64 %load_tmp, ptr %t_3, align 4
    %t_34 = load i64, ptr %t_3, align 4
    store i64 %t_34, ptr %x, align 4
    %t_05 = load i64, ptr %t_0, align 4
    %load_tmp6 = call i64 @field(i64 %t_05, i64 8)
    store i64 %load_tmp6, ptr %t_2, align 4
    %t_27 = load i64, ptr %t_2, align 4
    store i64 %t_27, ptr %y, align 4
    %x8 = load i64, ptr %x, align 4
    %y9 = load i64, ptr %y, align 4
    %addtmp = add i64 %x8, %y9
    store i64 %addtmp, ptr %t_1, align 4
    %t_110 = load i64, ptr %t_1, align 4
    ret i64 %t_110
  }

  $ ../bin/XML_llvm.exe -o tuple_gc_stress.s <<EOF
  > let rec make_list n acc =
  >   if n = 0 then acc else
  >   make_list (n - 1) (n, acc)
  > 
  > let main =
  >   let _ = print_gc_status in
  >   let result = make_list 10000 0 in
  >   let (head, tail) = result in
  >   let _ = print_gc_status in
  >   print_int head
  $ cat tuple_gc_stress.s
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_12 = alloca i64, align 8
    %t_11 = alloca i64, align 8
    %tail = alloca i64, align 8
    %t_13 = alloca i64, align 8
    %head = alloca i64, align 8
    %t_14 = alloca i64, align 8
    %t_10 = alloca i64, align 8
    %result = alloca i64, align 8
    %t_9 = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @print_gc_status to i64), i64 0)
    store i64 %closure_tmp, ptr %t_7, align 4
    %closure_tmp1 = call i64 @alloc_closure(i64 ptrtoint (ptr @make_list to i64), i64 2)
    %app_tmp = call i64 @apply1(i64 %closure_tmp1, i64 10000)
    store i64 %app_tmp, ptr %t_8, align 4
    %clos_as_i64 = ptrtoint ptr %t_8 to i64
    %app_tmp2 = call i64 @apply1(i64 %clos_as_i64, i64 0)
    store i64 %app_tmp2, ptr %t_9, align 4
    %t_93 = load i64, ptr %t_9, align 4
    store i64 %t_93, ptr %result, align 4
    %result4 = load i64, ptr %result, align 4
    store i64 %result4, ptr %t_10, align 4
    %t_105 = load i64, ptr %t_10, align 4
    %load_tmp = call i64 @field(i64 %t_105, i64 0)
    store i64 %load_tmp, ptr %t_14, align 4
    %t_146 = load i64, ptr %t_14, align 4
    store i64 %t_146, ptr %head, align 4
    %t_107 = load i64, ptr %t_10, align 4
    %load_tmp8 = call i64 @field(i64 %t_107, i64 8)
    store i64 %load_tmp8, ptr %t_13, align 4
    %t_139 = load i64, ptr %t_13, align 4
    store i64 %t_139, ptr %tail, align 4
    %closure_tmp10 = call i64 @alloc_closure(i64 ptrtoint (ptr @print_gc_status to i64), i64 0)
    store i64 %closure_tmp10, ptr %t_11, align 4
    %head11 = load i64, ptr %head, align 4
    call void @print_int(i64 %head11)
    store i64 0, ptr %t_12, align 4
    %t_1212 = load i64, ptr %t_12, align 4
    store i64 %t_1212, ptr %main, align 4
    ret i64 0
  }
  
  define i64 @make_list(i64 %n, i64 %acc) {
  entry:
    %t_5 = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %acc2 = alloca i64, align 8
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 4
    store i64 %acc, ptr %acc2, align 4
    %n3 = load i64, ptr %n1, align 4
    %eqtmp = icmp eq i64 %n3, 0
    store i1 %eqtmp, ptr %t_0, align 1
    %t_04 = load i64, ptr %t_0, align 4
    %cond = icmp ne i64 %t_04, 0
    br i1 %cond, label %then, label %else
  
  then:                                             ; preds = %entry
    %acc5 = load i64, ptr %acc2, align 4
    br label %ifcont
  
  else:                                             ; preds = %entry
    %n6 = load i64, ptr %n1, align 4
    %subtmp = sub i64 %n6, 1
    store i64 %subtmp, ptr %t_1, align 4
    %t_17 = load i64, ptr %t_1, align 4
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @make_list to i64), i64 2)
    %app_tmp = call i64 @apply1(i64 %closure_tmp, i64 %t_17)
    store i64 %app_tmp, ptr %t_2, align 4
    %n8 = load i64, ptr %n1, align 4
    %acc9 = load i64, ptr %acc2, align 4
    %tuple_vals_alloca = alloca i64, i64 2, align 8
    %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
    store i64 %n8, ptr %ptr_to_elem, align 4
    %ptr_to_elem10 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
    store i64 %acc9, ptr %ptr_to_elem10, align 4
    %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
    %tuple_tmp = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i64)
    store i64 %tuple_tmp, ptr %t_3, align 4
    %t_311 = load i64, ptr %t_3, align 4
    %clos_as_i64 = ptrtoint ptr %t_2 to i64
    %app_tmp12 = call i64 @apply1(i64 %clos_as_i64, i64 %t_311)
    store i64 %app_tmp12, ptr %t_4, align 4
    %t_413 = load i64, ptr %t_4, align 4
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %acc5, %then ], [ %t_413, %else ]
    store i64 %iftmp, ptr %t_5, align 4
    %t_514 = load i64, ptr %t_5, align 4
    ret i64 %t_514
  }

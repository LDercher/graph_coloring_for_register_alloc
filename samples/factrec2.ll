define i64 @function(i64 %arg) {
  %slota = alloca i64
  br label %block1

block1:
  %slotb = alloca i64
  br label %block2

block2:
  store i64 %arg, i64* %slotb
  br label %block3

block3:
  %valb = load i64, i64* %slotb
  br label %block4

block4:
  %valble0 = icmp sle i64 %valb, 0
  br i1 %valble0, label %then, label %else

then:
  store i64 1, i64* %slota
  br label %done

else:
  %valb2 = load i64, i64* %slotb
  br label %block5

block5:
  %valbminus1 = sub i64 %valb2, 1
  br label %block6

block6:
  %recursive = call i64 @function(i64 %valbminus1)
  br label %block7

block7:
  %multiplied = mul i64 %valb2, %recursive
  br label %block8

block8:
  store i64 %multiplied, i64* %slota
  br label %done

done:
  %vala2 = load i64, i64* %slota
  ret i64 %vala2
}

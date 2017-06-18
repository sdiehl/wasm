(module

  (func $fac-rec (param $n i64) (result i64)
    (if_else (i64.eq (get_local $n) (i64.const 0))
      (i64.const 1)
      (i64.mul
        (get_local $n)
        (call $fac-rec (i64.sub (get_local $n) (i64.const 1)))
      )
    )
  )

)

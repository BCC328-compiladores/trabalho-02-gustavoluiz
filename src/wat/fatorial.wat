(module
  (import "env" "print" (func $print (param i32)))
  (memory $0 1)
  (export "memory" (memory $0))
  (func $factorial (param $n i32) (result i32)
    (if (i32.le_s (local.get $n) (i32.const 1))
      (then
        (return (i32.const 1))
      )
      (else
        (return (i32.mul (local.get $n) (call $factorial (i32.sub (local.get $n) (i32.const 1)))))
      )
    )
    (i32.const 0)
  )
  (func $main
    (local $res i32)
    (local.set $res (call $factorial (i32.const 5)))
    (call $print (local.get $res))
  )
  (export "main" (func $main))
)
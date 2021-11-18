#lang play
(require "t1.rkt")


(test (typecheck '{5})
      'Num)

(test (typecheck '{#t})
      'Bool)

(test (typecheck '{{add1 4}})
      'Num)

(test/exn (typecheck '{{add1 #f}})
          "error:")

(test (typecheck '{{! #f}})
      'Bool)

(test/exn (typecheck '{{! 4}})
          "error:")

(test (typecheck '{{+ 1 3}})
      'Num)

(test/exn (typecheck '{{+ 3 #t}})
          "error:")

(test (typecheck '{{|| #f #t}})
      'Bool)

(test/exn (typecheck '{{&& #f 5}})
          "error:")

(test (typecheck '{{if #t 5 3}})
      'Num)

(test (typecheck '{{if #t #f #t}})
      'Bool)

(test/exn (typecheck '{{if #t #f 3}})
          "error:")

(test/exn (typecheck '{{if 3 2 3}})
          "error:")

(test (typecheck '{{if {|| #f #t} 5 3}})
      'Num)

(test (typecheck '{{+ 1 3}})
      'Num)

(test (typecheck '{{> 1 3}})
      'Bool)

(test/exn (typecheck '{
                       {== #f 3}
                       })
          "error:")

(test (typecheck '{
                   {with {{x 5}} {+ 1 x}}
                   })
      'Num)

(test (typecheck '{
                   {with {{x #f}} {&& #t #f}}
                   })
      'Bool)

(test (typecheck '{
                   {with {{x : Bool #t}} {&& #f #f}}
                   })
      'Bool)

(test/exn (typecheck '{
                         {with {{x : Bool 5}}{+ 2 x}}
                         })
          "error:")

(test (typecheck '{
                   {with {{x 5}} x}
                     })
      'Any)

(test (typecheck '{
                   {with {{x : Bool #t}{y : Num 10}} {if x 1 0}}
                   })
      'Num)

(test (typecheck '{
                   {define {f x} {+ 1 x}}
                   {f 6}
                   })
      'Any)

(test (typecheck '{
                   {define {f x} : Num {+ 1 x}}
                   {f 6}
                   })
      'Num)

(test/exn (typecheck '{
                       {define {f x} : Bool {+ 1 x}}
                       {f 6}
                       })
          "error:")

(test (typecheck '{
                   {define {f {x : Num}} {+ 1 x}}
                   {f 6}
                   })
      'Any)

(test/exn (typecheck '{
                       {define {f {x : Bool}} {+ 1 x}}
                       {f 6}
                       })
          "error:")

(test (typecheck '{
                   {define {f x y} {+ y x}}
                   {f 6 7}
                   })
      'Any)

(test (typecheck '{
                   {define {f x y} : Num {+ y x}}
                   {f 6 7}
                   })
      'Num)

(test (typecheck '{
                   {define {f {x : Num} {y : Num}} : Num {+ y x}}
                   {f 6 7}
                   })
      'Num)

(test/exn (typecheck '{
                       {define {f {x : Bool} y} {+ 1 x}}
                       {f 6}
                       })
          "error:")

(test (typecheck '{
                   {define {f {x : Num} y} : Num {+ y x}}
                   {f 6 7}
                   })
      'Num)

(test (typecheck '{
                   {define {f {x : Num} y} : Num {+ y x}}
                   {define {pow z} {* z z}}
                   {pow {f 6 7}}
                   })
      'Any)

(test (typecheck '{
                   {with {{x : Num 5} {y : Num 10}} {+ x y}}
                   })
      'Num)

(test (typecheck '{
                   {define {gt42 x} : Bool {> x 42}}
                   {gt42 43}
                   })
      'Bool)

(test (typecheck '{
                   {define {id {x : Num}} x}
                   {id 5}
                   })
      'Any)

(test (typecheck '{
                   {define {add2 {x : Num}} {+ x 2}}
                   {with {{oops #f}}
                         {add2 oops}}
                   })
      'Any)

(test (typecheck '{
                   3
                   })
      'Num)

(test (typecheck '{
                   {define {f {p : Bool}} {&& p {! p}}}
                   {f {> 3 4}}
                   })
      'Any)

(test/exn (typecheck '{
                       {define {one {x : Num}} 1}
                       {one #t}
                       })
          "error:")

(test/exn (typecheck '{
                       {> 10 #t}
                       })
          "error:")

(test/exn (typecheck '{
                       {if 73 #t #t}
                       })
          "error:")

(test/exn (typecheck '{
                       {with {{x 5} {y : Num #t} {z 42}}
                             z}
                       })
          "error:")

; Llamando a funcion que no esta definida
(test/exn (run '{
                 {define {fun x}{- 2 x}}
                 {notfun 3}
                 }
           )
          "error:")

(test (run '{; Programa de Ejemplo: Fibonacci
             {define {fib {x : Num}}: Num
               {if {|| {== x 0}
                       {== x 1}}
                   1
                   {+ {fib {- x 1}} {fib {- x 2}}}
                   }}
             {fib 11}
             })
      144)

; Error dinamico
(test/exn (run '{
                 {define {fib x}
                   {if {|| {== x 0}
                           {== x 1}}
                       1
                       {+ {fib {- x 1}} {fib {- x 2}}}
                       }}
                 {fib #f}
                 }
           )
          "error:")

; Error estatico
(test/exn (run '{
                 {define {fib {x : Num}}: Num
                   {if {|| {== x 0}
                           {== x 1}}
                       1
                       {+ {fib {- x 1}} {fib {- x 2}}}
                       }}
                 {fib #f}
                 }
           )
          "error:")

; De todas maneras me puedo saltar el checkeo estatico al pasar un tipo Any
; Haciendo que el programa arroje error dinamicamente en runtime
;
; Demostrando que el tener un tipo comodin por si no se tipea una variable
; otorga flexibilidad, pero provoca que se pueda "engañar" el typecheck
(test/exn (run '{
                 {define {fib {x : Num}}: Num
                   {if {|| {== x 0}
                           {== x 1}}
                       1
                       {+ {fib {- x 1}} {fib {- x 2}}}
                       }}
                 {with {{any : Any #f}}
                       {fib any}}
                 }
           )
          "error:")

(test (run '{
             {define {is-multiple-of {n : Num}{m : Num}} : Bool
               {if {< n 0}
                   #f
                   {if {== n 0}
                       #t
                       {is-multiple-of {- n m} m}}}}
             {is-multiple-of 21 7}
             })
      #t)

(test (run '{
             {define {is-multiple-of {n : Num}{m : Num}} : Bool
               {if {< n 0}
                   #f
                   {if {== n 0}
                       #t
                       {is-multiple-of {- n m} m}}}}
             {is-multiple-of 21 4}
             })
      #f)

(test (run '{
             {define {is-multiple-of {n : Num}{m : Num}} : Bool
               {if {< n 0}
                   #f
                   {if {== n 0}
                       #t
                       {is-multiple-of {- n m} m}}}}
             {define {even x} : Bool {is-multiple-of x 2}}
             {even 4}
             })
      #t)

(test (run '{
             {define {is-multiple-of {n : Num}{m : Num}} : Bool
               {if {< n 0}
                   #f
                   {if {== n 0}
                       #t
                       {is-multiple-of {- n m} m}}}}
             {define {even x} : Bool {is-multiple-of x 2}}
             {even 7}
             })
      #f)
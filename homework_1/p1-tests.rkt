#lang play
(require "t1.rkt")

(test (run '{
             {* 4 2}
             })
      8)

(test (run '{
             {/ 10 2}
             })
      5)

(test (run '{
             {add1 10}
             })
      11)

(test (run '{
             {sub1 10}
             })
      9)

(test/exn (run '{
                 {* 4 #f}
                 }
           )
          "error:")

(test/exn (run '{
                 {add1 #f}
                 }
           )
          "error:")

(test/exn (run '{
                 {sub1 #t}
                 }
           )
          "error:")

(test (run '{
             {&& #f #t}
             })
      #f)

(test (run '{
             {|| #f #t}
             })
      #t)

(test (run '{
             {== 4 4}
             })
      #t)

(test (run '{
             {== 5 2}
             })
      #f)

(test (run '{
             {< 2 4}
             })
      #t)

(test (run '{
             {< 8 2}
             })
      #f)

(test (run '{
             {> 78 4}
             })
      #t)

(test (run '{
             {> -6 2}
             })
      #f)


(test (run '{
             {! #f}
             })
      #t)

(test/exn (run '{
                 {&& 4 #f}
                 }
           )
          "error:")

(test/exn (run '{
                 {|| #t 34}
                 }
           )
          "error:")

(test/exn (run '{
                 {! 2}
                 }
           )
          "error:")

(test (run '{
             {if #t
                 2
                 4}
             })
      2)

(test (run '{
             {if #f
                 2
                 4}
             })
      4)

(test/exn (run '{
                 {if 32
                     2
                     4}
                 }
           )
          "error:")


(test (run '{
             {if {> 100 1}
                 2
                 4}
             })
      2)

(test (run '{
             {if {< 1000 999}
                 2
                 4}
             })
      4)

(test (run '{
             {if #f
                 #t
                 #f}
             })
      #f)

(test (run '{
             {define {f x y z w}{+ w {+ z {+ x y}}}}
             {f 10 5 1 9}
             })
      25)

(test/exn (run '{
                 {define {f x y}{+ x y}}
                 {f 10 5 6}
                 }
           )
          "error:")

(test/exn (run '{
                 {define {f x y}{+ x y}}
                 {f 10}
                 }
           )
          "error:")

(test (run '{
             {with {{x 10} {y -3}}
                    {+ x y}}
             })
      7)

(test (run '(
             {with {}
                   {+ 2 3}}
             ))
      5)

(test (run '(
             {define {f}{+ 7 5}}
             {f}
             ))
      12)

(test (run '({+ 5 5}))
      10)

(test (run '(
             {define {f x}{+ x 5}}
             {f 2}
             ))
      7)

(test (run '{; Programa de Ejemplo 1
             {define {sum x y z} {+ x {+ y z}}}
             {define {max x y} {if {< x y} y x}}
             {with {{x 9}}
                   {sum {max x 6} 2 -10} }
             })
      1)

(test (run '{; Programa de Ejemplo 2
             {with {{x 5} {y 7} {z 42}}
                   z}
             })
      42)

(test (run '{; Programa de Ejemplo 3
             {define {triple x} {* 3 x}}
             {define {add2 x} {+ 2 x}}
             {add2 {triple 2}}
             })
      8)


(test/exn (run '{
                 {+ 1 #f}
                 }
           )
          "error:")


(test (run '{; Programa de Ejemplo: Fibonacci
             {define {fib x} {if {|| {== x 0}
                                     {== x 1}}
                                 1
                                 {+ {fib {- x 1}} {fib {- x 2}}}
                                 }}
             {fib 11}
             })
      144)

; Debido a la implementacion de la parte 2 debemos declarar un variable
; de tipo Any para ver los errores dinamicos, directa o indirectamente

(test/exn (run '{
                 {with {{x #f}} {+ x 5}}
                 }
           )
          "error:")

(test (run '{
             {with {{x 8}} {- x 5}}
             })
      3)

(test/exn (run '{
                 {with {{z 5}} {&& z #f}}
                 }
           )
          "error:")

(test (run '{
             {with {{a #f}} {|| a #t}}
             })
      #t)

(test (run '{
             {define {fun1 x}{+ x 1}}
             {define {fun2 x y} {* x {fun1 y}}}
             {define {fun3 x y z} {fun2 {fun1 x} {+ y {add1 z}}}}
             {fun3 1 2 3}
             })
      14)


(test/exn (run '{
                 {define {fun1 x y}{* 2 {* x y}}}
                 {define {fun2 s} {> 0 s}}
                 {with {{x {+ 3 3}}
                        {y {fun2 6}}}
                       {+ x {- 1 y}}}
                 }
           )
          "error:")

(test (run '{
                 {define {fun1 x y}{* 2 {* x y}}}
                 {define {fun2 s} {> 0 s}}
                 {with {{x {+ 3 3}}
                        {y {fun2 6}}}
                       {if y
                           {- x 2}
                           {* x 2}}}
                 })
      12)


; Llamadas a funciones anidadas
(test (run '{
             {define {fun1 x}{+ x 1}}
             {define {fun2 x y} {* x {fun1 y}}}
             {define {fun3 x y z} {fun2 {fun1 x} {+ y {add1 z}}}}
             {fun3 {fun2 {fun1 1}
                         {fun1 2}}
                   {fun2 {fun1 3}
                         {fun1 4}}
                   {fun2 {fun1 5}
                         {fun1 6}}}
             })
      666)


; with anidados
(test (run '{
             {with {{x {with {{a {with {{c 1}}{+ c c}}}
                              {b {with {{c 1}}{* c c}}}}
                             {+ a b}}}
                    {y {with {{a {with {{c 1}}{* c c}}}
                              {b {with {{c 1}}{- c c}}}}
                             {+ a b}}}
                    {z {with {{a {with {{c 1}}{+ c c}}}
                              {b {with {{c 1}}{+ c c}}}}
                             {+ a b}}}}
                   {if {== x y}
                       0
                       {if {== x z}
                           1
                           {if {== y z}
                               2
                               3}}}}
             })
      3)

; with y mas operadores en declaracion de funciones
(test (run '{
             {define {fun1 x}{* x x}}
             {define {fun2 x y} {if {&& {> x y} {== x y}}
                                    #t
                                    #f}}
             {define {fun3 x y z}
               {with {{a {+ x 1}}{b {- y 2}}{c {* z 4}}}
                     {with {{q {+ a b}}
                            {p {- c z}}}
                           {+ q p}}}}
             {if {fun2 {fun1 10}{fun3 1 2 3}}
                 {fun1 10}
                 {fun3 1 2 3}}
             })
      11)
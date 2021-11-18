#lang play
(require "t1.rkt")

(test/exn (typecheck '{
                       {define {add x y} : Num {+ x y}}
                       {define {oh-no {x @ add} y}
                         #t}
                       {oh-no 21 21}
                       })
          "error:")

(test (typecheck '{
                   {define {positive x} : Bool {> x 0}}
                   {define {div {x : Num @ positive} y} : Num
                     {/ y x}}
                   {div 5 3}
                   })
      'Num)

(test (run '{
             {define {positive x} : Bool {> x 0}}
             {define {div {x : Num @ positive} y} : Num
               {/ y x}}
             {div 5 3}
             })
      (/ 3 5))

(test/exn (run '{
                 {define {positive x} : Bool {> x 0}}
                 {define {div {x : Num @ positive} y} : Num
                   {/ y x}}
                 {div -3 3}
                 })
          "error:")

(test (run '{
             {define {lt100 x} {< x 100}}
             {define {positive x} : Bool {> x 0}}
             {define {percentage? x} : Bool {&& {lt100 x} {positive x}}}
             {define {calc {x @ positive} {y @ percentage?}}
               {/ {* y y} x}}
             {calc 25 3}
             })
      (/ 9 25))

(test/exn (run '{
                 {define {add x y} : Num {+ x y}}
                 {define {oh-no {x @ add} y}
                   #t}
                 {oh-no 21 21}
                 })
          "error:")

; contrato no definido
(test/exn (run '{
                 {define {div {x : Num @ positive} y} : Num
                   {/ y x}}
                 {div -3 3}
                 })
          "error:")

; Error al especificar un tipo para el parametro del contrato
(test/exn (run '{
                 {define {positive {x : Num}} : Bool {> x 0}}
                 {define {div {x : Num @ positive} y} : Num
                   {/ y x}}
                 {div -3 3}
                 })
          "error:")

(test/exn (run '{
                 {define {positive {x : Bool}} : Bool {> x 0}}
                 {define {div {x : Num @ positive} y} : Num
                   {/ y x}}
                 {div -3 3}
                 })
          "error:")

; No hay error al especificar el parametro del contrato como Any
(test (run '{
             {define {positive {x : Any}} : Bool {> x 0}}
             {define {div {x : Num @ positive} y} : Num
               {/ y x}}
             {div 5 3}
             })
      (/ 3 5))

(test (run '{
             {define {is-multiple-of {n : Num}{m : Num}} : Bool
               {if {< n 0}
                   #f
                   {if {== n 0}
                       #t
                       {is-multiple-of {- n m} m}}}}
             {define {even x} : Bool {is-multiple-of x 2}}
             {define {less100 {a @ even}}: Bool
               {< a 100}}
             {less100 34}
             })
      #t)

; Error del contrato even o par
(test/exn (run '{
             {define {is-multiple-of {n : Num}{m : Num}} : Bool
               {if {< n 0}
                   #f
                   {if {== n 0}
                       #t
                       {is-multiple-of {- n m} m}}}}
             {define {even x} : Bool {is-multiple-of x 2}}
             {define {less100 {a @ even}}: Bool
               {< a 100}}
             {less100 33}
             })
          "error:")

; Multiple contratos
(test (run '{
             {define {is-multiple-of {n : Num}{m : Num}} : Bool
               {if {< n 0}
                   #f
                   {if {== n 0}
                       #t
                       {is-multiple-of {- n m} m}}}}
             {define {positive {x : Any}} : Bool {> x 0}}
             {define {even {x @ positive}} : Bool {is-multiple-of x 2}}
             {define {negative {x : Any}} : Bool {< x 0}}
             {define {odd x} : Bool {! {is-multiple-of x 2}}}
             {define {less100 {a @ even}}: Bool
               {< a 100}}
             {define {nested {x @ less100}{y @ odd}{z @ negative}}
               {+ {* x y} z}}
             {nested 20 11 -10}
             })
      210)

; Error del contrato positive
(test/exn (run '{
             {define {is-multiple-of {n : Num}{m : Num}} : Bool
               {if {< n 0}
                   #f
                   {if {== n 0}
                       #t
                       {is-multiple-of {- n m} m}}}}
             {define {positive {x : Any}} : Bool {> x 0}}
             {define {even {x @ positive}} : Bool {is-multiple-of x 2}}
             {define {negative {x : Any}} : Bool {< x 0}}
             {define {odd x} : Bool {! {is-multiple-of x 2}}}
             {define {less100 {a @ even}}: Bool
               {< a 100}}
             {define {nested {x @ less100}{y @ odd}{z @ negative}}
               {+ {* x y} z}}
             {nested -20 11 -10}
             })
          "error:")

; Error del contrato even
(test/exn (run '{
             {define {is-multiple-of {n : Num}{m : Num}} : Bool
               {if {< n 0}
                   #f
                   {if {== n 0}
                       #t
                       {is-multiple-of {- n m} m}}}}
             {define {positive {x : Any}} : Bool {> x 0}}
             {define {even {x @ positive}} : Bool {is-multiple-of x 2}}
             {define {negative {x : Any}} : Bool {< x 0}}
             {define {odd x} : Bool {! {is-multiple-of x 2}}}
             {define {less100 {a @ even}}: Bool
               {< a 100}}
             {define {nested {x @ less100}{y @ odd}{z @ negative}}
               {+ {* x y} z}}
             {nested 21 11 -10}
             })
          "error:")

; Error del contrato negative
(test/exn (run '{
             {define {is-multiple-of {n : Num}{m : Num}} : Bool
               {if {< n 0}
                   #f
                   {if {== n 0}
                       #t
                       {is-multiple-of {- n m} m}}}}
             {define {positive {x : Any}} : Bool {> x 0}}
             {define {even {x @ positive}} : Bool {is-multiple-of x 2}}
             {define {negative {x : Any}} : Bool {< x 0}}
             {define {odd x} : Bool {! {is-multiple-of x 2}}}
             {define {less100 {a @ even}}: Bool
               {< a 100}}
             {define {nested {x @ less100}{y @ odd}{z @ negative}}
               {+ {* x y} z}}
             {nested 20 11 10}
             })
          "error:")

; error del contrato less100
(test/exn (run '{
             {define {is-multiple-of {n : Num}{m : Num}} : Bool
               {if {< n 0}
                   #f
                   {if {== n 0}
                       #t
                       {is-multiple-of {- n m} m}}}}
             {define {positive {x : Any}} : Bool {> x 0}}
             {define {even {x @ positive}} : Bool {is-multiple-of x 2}}
             {define {negative {x : Any}} : Bool {< x 0}}
             {define {odd x} : Bool {! {is-multiple-of x 2}}}
             {define {less100 {a @ even}}: Bool
               {< a 100}}
             {define {nested {x @ less100}{y @ odd}{z @ negative}}
               {+ {* x y} z}}
             {nested 1000 11 -10}
             })
          "error:")
#lang play

;(print-only-errors #t)

(require "main.rkt")

;; Test sub-module.
;; See http://blog.racket-lang.org/2012/06/submodules.html

;this tests should never fail; these are tests for MiniScheme+
(module+ test

  ;; Tests base entregados
  
  (test (run '{+ 1 1}) 2)

  (test (run '{{fun {x y z} {+ x y z}} 1 2 3}) 6)

  (test (run '{< 1 2}) #t)

  (test (run '{local {{define x 1}}
                x}) 1)

  (test (run '{local {{define x 2}
                      {define y {local {{define x 1}} x}}}
                {+ x x}}) 4)

  ;; datatypes
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {List? {Empty}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Empty}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {List? {Cons 1 2}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Cons? {Cons 1 2}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Cons 1 2}}})
        #f)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Empty}}}) #t)

  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Cons? {Empty}}})
        #f)

  ;; match
  (test (run '{match 1 {case 1 => 2}}) 2)

  (test (run '{match 2
                {case 1 => 2}
                {case 2 => 3}})
        3)

  (test (run '{match #t {case #t => 2}}) 2)

  (test (run '{match #f
                {case #t => 2}
                {case #f => 3}})
        3)

  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}
                      {define pred {fun {n}
                                        {match n
                                          {case {Zero} => {Zero}}
                                          {case {Succ m} => m}}}}}
                {Succ? {pred {Succ {Succ {Zero}}}}}})
        #t)
  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}
                      {define pred {fun {n}
                                        {match n
                                          {case {Zero} => {Zero}}
                                          {case {Succ m} => m}}}}}
                {Succ? {pred {Succ {Succ {Zero}}}}}}) #t)

;; Tests del enunciado y creados para probar funcionalidades
  
  (test (pretty-printing (structV 'Nat 'Succ (list
                                              (structV 'Nat 'Zero empty))))
        "{Succ {Zero}}")

  (test (run '{local {{datatype Nat 
                  {Zero} 
                  {Succ n}}
                {define pred {fun {n} 
                               {match n
                                 {case {Zero} => {Zero}}
                                 {case {Succ m} => m}}}}}
          {pred {Succ {Succ {Succ {Zero}}}}}} "ppwu")
        "{Succ {Succ {Zero}}}")

  ;; Tests para probar el print "ppwu" de otras estructuras
  (test (run '{local {{datatype BinTree 
                  {Leaf} 
                  {Node v r l}}}
          {Node 5
                {Node 3
                      {Node 2
                            {Leaf}
                            {Leaf}}
                      {Node 4
                            {Leaf}
                            {Leaf}}}
                {Node 7
                      {Node 6
                            {Leaf}
                            {Leaf}}
                      {Node 8
                            {Leaf}
                            {Leaf}}}}} "ppwu")
        "{Node 5 {Node 3 {Node 2 {Leaf} {Leaf}} {Node 4 {Leaf} {Leaf}}} {Node 7 {Node 6 {Leaf} {Leaf}} {Node 8 {Leaf} {Leaf}}}}")

  (test (run '{local {{datatype Places 
                  {First f} 
                  {Second f s}
                  {Third f s t}
                  {Fourth f s t fr}}}
          {First {Second {Third {Fourth 1
                                        2
                                        3
                                        4}
                                5
                                6}
                         7}}} "ppwu")
        "{First {Second {Third {Fourth 1 2 3 4} 5 6} 7}}")
  
  ;; Tests para probar la incorporacion nativa de la estructura List
  
  (test (run '{Empty? {Empty}})
        #t)

  (test (run '{List? {Cons 1 2}})
        #t)

  (test (run '{length {Cons 1 {Cons 2 {Cons 3 {Empty}}}}})
        3)

 ;; Tests para verificar el azucar sintactico al parsear una lista 
(test (parse '{list})
      (parse '{Empty})
      )

(test (parse '{list 1})
      (parse '{Cons 1 {Empty}})
      )

(test (parse '{list 1 2})
      (parse '{Cons 1 {Cons 2 {Empty}}})
      )

 (test (parse '{list 1 2 3})
      (parse '{Cons 1 {Cons 2 {Cons 3 {Empty}}}})
      )

  (test (parse '{list 1 2 3 4})
      (parse '{Cons 1 {Cons 2 {Cons 3 {Cons 4 {Empty}}}}})
      )

  ;; Tests para verficar el usos del azucar sintactico de la lista en lugar del
  ;; value en el match
  
  (test (run '{match {list {+ 1 1} 4 6}
          {case {Cons h r} => h}
          {case _ => 0}})
        2)

  (test (run '{match {list}
          {case {Cons h r} => h}
          {case _ => 0}})
        0)

  (test (run '{match {list 1 {list 2 3} 4}
          {case {Cons p {Cons {Cons 2 {Cons 3 {Empty}}} {Cons 4 {Empty}}}} => #t}
          {case {Cons h r} => h}
          {case _ => 0}})
        #t)

  ;; Tests para verificar el azucar sintactico al parsear una lista
  ;; en los casos de un pattern matching
  {test (parse-pattern '{list})
        (parse-pattern '{Empty})
        }

  {test (parse-pattern '{list a})
        (parse-pattern '{Cons a {Empty}})
        }

  (test (parse-pattern '{list a b})
        (parse-pattern '{Cons a {Cons b {Empty}}})
        )

  (test (parse-pattern '{list {list a}})
        (parse-pattern '{Cons {Cons a {Empty}} {Empty}})
        )

  ;; Tests para verificar el azucar sintactico en lugar de case en un match

  (test (run '{match {list 2 {list 4 5} 6}
          {case {list a {list b c} d} => c}})
        5
        )

  (test (run '{match {list {list 4 5}}
          {case {list} => 0}
          {case {list a {list b c} d} => c}
          {case {list {list a b}} => 3}})
        3
        )
  
  ;; Tests para probar el print con "pp" y tambien los otros print
  (test (run '{list})
        (structV 'List 'Empty '())
        )

  (test (run '{list} "ppwu")
        "{Empty}"
        )

  (test (run '{list} "pp")
        "{list}")

  (test (run '{list 1 2 3 4} "ppwu")
        "{Cons 1 {Cons 2 {Cons 3 {Cons 4 {Empty}}}}}"
        )

  (test (run '{list 1 2 3 4} "pp")
        "{list 1 2 3 4}"
        )

  (test (run '{list 1 {list} 2 {list 21 22} 3} "ppwu")
        "{Cons 1 {Cons {Empty} {Cons 2 {Cons {Cons 21 {Cons 22 {Empty}}} {Cons 3 {Empty}}}}}}"
        )

  (test (run '{list 1 {list} 2 {list 21 22} 3} "pp")
        "{list 1 {list} 2 {list 21 22} 3}"
        )

  (test (run '{list 1 4 6} "pp")
        "{list 1 4 6}"
        )

  ;; Test para probar lazyness (si se especifica)
  
  (test/exn (run '{{fun {x y} x} 1 {/ 1 0}})
          "division by zero"
          )

  (test (run '{{fun {x {lazy y}} x} 1 {/ 1 0}})
        1
        )
  
  (test/exn (run '{{fun {x {lazy y}} x} {/ 1 0} 1})
          "division by zero"
          )

  (test/exn (run '{local {{datatype T 
                  {C a}}
                {define x {C {/ 1 0}}}}
          {T? x}})
          "division by zero"
          )
  
  (test (run '{local {{datatype T 
                  {C {lazy a}}}
                {define x {C {/ 1 0}}}}
          {T? x}})
        #t
        )

  (test/exn (run '{local {{datatype T 
                        {C a b}}
              {define x {C {/ 88 0} 2}}}
        {match x
          {case {C z q} => q}}})
          "division by zero"
          )

  (test (run '{local {{datatype T 
                        {C {lazy a} b}}
              {define x {C {/ 88 0} 2}}}
        {match x
          {case {C z q} => q}}})
        2
        )

  (test/exn (run '{local {{datatype T 
                        {C {lazy a} b}}
              {define x {C {/ 88 0} 2}}}
        {match x
          {case {C z q} => z}}})
          "division by zero"
          )

  (test (run '{local {{datatype T {C a {lazy b}}}
                {define x {C  0 {+ 1 2}}}}
               x} "pp")
        "{C 0 3}"
        )

  (test (structV?
         (run '{local {{datatype T {C a {lazy b}}}
                {define x {C  0 {+ 1 2}}}}
               x}))
        #t
        )

  (test/exn (run '{local {{datatype T {C a {lazy b}}}
                {define x {C  0 {/ 1 0}}}}
               x} "pp")
          "division by zero"
          )

  (test (structV?
         (run '{local {{datatype T {C a {lazy b}}}
                {define x {C  0 {/ 1 0}}}}
               x}))
        #t
        )

  ;; Tests para probar lazyness en caso de match y usando estructuras en los cases

  (test/exn (run '{local {{datatype T 
                        {C {lazy a} b}}
              {define x {C 2 {/ 88 0}}}}
        {match x
          {case {C z {C r t}} => z}
          {case {C h q} => h}}})
          "division by zero"
          )

  (test/exn (run '{local {{datatype T 
                        {C a {lazy b}}}
              {define x {C 2 {/ 88 0}}}}
        {match x
          {case {C z {C r t}} => z}
          {case {C h q} => h}}})
          "division by zero"
          )
  
  (test (run '{local {{datatype T 
                  {C {lazy a}}}
                {define x {C {/ 1 0}}}}
          {match x
            {case {C a} => #t}}})
        #t
        )

  (test/exn (run '{local {{datatype T 
                  {C {lazy a}}}
                {define x {C {/ 1 0}}}}
          {match x
            {case {C a} => a}}})
          "division by zero"
          )

  ;; Tests de stream
  
  (test (run `{local {,stream-data ,make-stream ,stream-hd ,ones}
                {stream-hd ones}})
        1)

  (test (run `{local {,stream-data ,make-stream
                             ,stream-hd ,stream-tl ,ones}
          {stream-hd {stream-tl ones}}})
        1)

  (test (run `{local ,stream-lib
          {local {,ones}
            {stream-take 10 ones}}} "pp")
        "{list 1 1 1 1 1 1 1 1 1 1}")

  ; stream de zeros
  (def zeros '{define zeros {make-stream 0 zeros}})
  
  ; Definicion Stream infinito de los numeros naturales en MiniScheme+
  (def naturals '{define naturals
             {make-stream 1 {stream-zipWith {fun {n m}
                                               {+ n m}}
                                          naturals
                                          ones}}})

  ; Funcion que genera un stream con los multiplos de n
 (def table-n '{define table-n
                  {fun {n}
                       {stream-zipWith
                        {fun {a b}
                             {* a {* b n}}}
                        {make-stream 0 naturals}
                        ones}}})

  ; Definicion de funciones 2 de MiniScheme+
  (def stream-lib2 (list zeros
                         naturals
                        ))
  
  ;; Tests para Probar otros streams juntos con el stream-take y
  ;; zipWith con otras funciones, streams

  (test (run `{local ,stream-lib
          {local {,ones ,stream-zipWith}
            {stream-take 10
                         {stream-zipWith
                          {fun {n m}
                               {+ n m}}
                          ones
                          ones}}}} "pp")
        "{list 2 2 2 2 2 2 2 2 2 2}")

  (test (run `{local ,stream-lib
          {local ,stream-lib2
            {stream-take 15 zeros}}} "pp")
        "{list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0}")

  {test (run `{local ,stream-lib
          {local {,stream-zipWith ,ones , naturals}
            {stream-take 20 naturals}}} "pp")
        "{list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20}"}

  (test (run `{local ,stream-lib
          {local {,stream-zipWith ,ones , naturals, table-n}
            {stream-take 12 {table-n 5}}}} "pp")
        "{list 0 5 10 15 20 25 30 35 40 45 50 55}")


  ;; Tests para Probar otros tamaños de fibs

  (test (run `{local ,stream-lib
          {local {,stream-zipWith ,fibs}
            {stream-take 10 fibs}}} "pp")
        "{list 1 1 2 3 5 8 13 21 34 55}")

  (test (run `{local ,stream-lib
          {local {,stream-zipWith ,fibs}
            {stream-take 4 fibs}}} "pp")
        "{list 1 1 2 3}")

  (test (run `{local ,stream-lib
          {local {,stream-zipWith ,fibs}
            {stream-take 40 fibs}}} "pp")
        "{list 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040 1346269 2178309 3524578 5702887 9227465 14930352 24157817 39088169 63245986 102334155}")

  
  ;; Tests para Probar merge con otros streams

  (test (run `{local ,stream-lib
               {local {,stream-take ,merge-sort ,fibs ,stream-zipWith}
                 {stream-take 10 {merge-sort fibs fibs}}}} "pp")
        "{list 1 1 1 1 2 2 3 3 5 5}")


  (test (run `{local ,stream-lib
               {local {,stream-take ,merge-sort ,fibs ,stream-zipWith, ones, naturals}
                 {stream-take 20 {merge-sort fibs naturals}}}} "pp")
        "{list 1 1 1 2 2 3 3 4 5 5 6 7 8 8 9 10 11 12 13 13}")

  (test (run `{local ,stream-lib
               {local {,stream-take ,merge-sort ,stream-zipWith, ones, naturals, table-n}
                 {stream-take 9 {merge-sort naturals {table-n 2}}}}} "pp")
       "{list 0 1 2 2 3 4 4 5 6}")

  (test (run `{local ,stream-lib
               {local {,stream-take ,merge-sort ,stream-zipWith, ones, naturals, table-n}
                 {stream-take 19 {merge-sort {table-n 21} {table-n 19}}}}} "pp")
        "{list 0 0 19 21 38 42 57 63 76 84 95 105 114 126 133 147 152 168 171}")
  
  )

; !!!!!!
; Los siguientes test no se me ejecutan, asi que se agregaron a los tests anteriores


;tests for extended MiniScheme+
;uncomment sanity tests when you are ready
#;(module+ sanity-tests
    (test (run '{local {{datatype Nat
                  {Zero}
                  {Succ n}}
                {define pred {fun {n}
                               {match n
                                 {case {Zero} => {Zero}}
                                 {case {Succ m} => m}}}}}
          {pred {Succ {Succ {Zero}}}}} "ppwu") "{Succ {Zero}}")

(test (run
 `{local ,stream-lib
          {local {,ones ,stream-take}
            {stream-take 11 ones}}} "pp") "{list 1 1 1 1 1 1 1 1 1 1 1}")

(test (run `{local ,stream-lib
          {local {,stream-zipWith ,fibs}
            {stream-take 10 fibs}}} "pp") "{list 1 1 2 3 5 8 13 21 34 55}")

(test (run `{local ,stream-lib
          {local {,ones ,stream-zipWith}
            {stream-take 10
                         {stream-zipWith
                          {fun {n m}
                               {+ n m}}
                          ones
                          ones}}}} "pp")  "{list 2 2 2 2 2 2 2 2 2 2}")
(test
(run `{local ,stream-lib
               {local {,stream-take ,merge-sort ,fibs ,stream-zipWith}
                 {stream-take 10 {merge-sort fibs fibs}}}} "pp")   "{list 1 1 1 1 2 2 3 3 5 5}"))


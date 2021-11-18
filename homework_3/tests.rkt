#lang play
(require "main.rkt")
(print-only-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 TESTS BASE                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '(+ 1 2)) 3)
(test (run-val '(< 1 2)) #t)
(test (run-val '(- 2 1)) 1)
(test (run-val '(* 2 3)) 6)
(test (run-val '(= (+ 2 1) (- 4 1))) #t)
(test (run-val '(and #t #f)) #f)
(test (run-val '(or #t #f)) #t)
(test (run-val '(not (not #t))) #t)
(test (run-val '(if (not #f) (+ 2 1) 4)) 3)
(test (run-val '(local ([define x 5])
              (seqn {+ x 1}
                    x))) 5)

;; Ejemplos del enunciado
; NO DESCOMENTAR, estan agregados con los demas test abajo :D
#;(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get x) (+ (get y) z)))
                          (method set-x (val) (set x val))
                          (method get-y () (get y))))]
            (seqn
             (send o set-x (+ 1 3))
             (+ (send o sum 3) (send o get-y)))))
      11)

#;(test (run-val
       '(local
            [(define a
               (object
                (method auto-apply (o)
                        (send o apply o))
                (method foo 5)
                ))
             (define o (send a auto-apply
                             (object
                              (method apply (other) (send other apply2 this))
                              (method apply2 (other) this)
                              (method foo () 42))))]
          (send o foo)))
      42)

#;(test (run-val '(local
              [(define smart-computer (object
                                       (method secret? (something) 42)))
               (define everything (object))
               (define oracle (object : smart-computer))]
               (send oracle secret? everything)))
      42)

#;(run-val '(local
              [(define seller (object
                               (method multiplier () 1)
                               (method price (item-number)
                                       (* item-number (send this multiplier)))))
               (define broker (object : seller
                                      (method multiplier () 2)))]
               (send broker price 3)))

#;(test (run-val '(local
                    ([define x (object
                                (field z 3)
                                (method get () (get z)))]
                     [define y (object : x)])
                  (send y get)))
      3)

#;(test/exn (run-val '(local
                        ([define x (object
                                    (field z 3)
                                    (method get () (get z)))]
                         [define y (object
                                    : x
                                    (method get () (get z)))])
                      (send y get)))
          "field not found")

;; A simple monotone counter
#;(define counter '(object
                  (field count 0)
                  (method incr () (set count (+ 1 (get count))))
                  (method get () (get count))))

#;(define (incrs-multiply x y)
  `(seqn
    (send ,y incr)
    (seqn
     (send ,x incr)
     (seqn
      (send ,x incr)
      (* (send ,x get) (send ,y get))
      ))))

#;(test (run-val
       `(local ([define c ,counter])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

#;(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      16)

#;(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (deep-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Prueba de get simple
(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method get-y () (get y))))]
            (send o get-y)))
      2)

(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method get-x () (get x))))]
            (send o get-x)))
      1)

;Prueba de varios get
(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method get-x () (get x))
                          (method get-y () (get y))))]
            (+ (send o get-y)
               (send o get-x))))
      3)

;Prueba se set simple
(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method get-x () (get x))
                          (method set-x (val) (set x val))))]
                  (seqn
                   (send o set-x 10)
                   (send o get-x))))
      10)

;Prueba de varios setters y getters
(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method get-x () (get x))
                          (method get-y () (get y))
                          (method set-x (val) (set x val))
                          (method set-y (val) (set y val))))]
                  (seqn
                   (send o set-x 10)
                   (seqn (send o set-y 100)
                         (+ (send o get-x)
                            (send o get-y))))))
      110)

;Prueba de metodo simple
(test (run-val '(local
              [(define o (object
                          (method test (val) (* 3 val))))]
                  (send o test 10)))
      30)

(test (run-val '(local
              [(define o (object
                          (method test (val1 val2 val3) (+
                                                         val1
                                                         (* val2 val3)))))]
                  (send o test 5 10 100)))
      1005)

;Prueba de metodos que llaman a otros metodos y de this
(test (run-val '(local
              [(define o (object
                          (method test (val) (+ val
                                                (send this test2 10)))
                          (method test2 (val) (* val val))))]
                  (send o test 10)))
      110)

;Prueba de un metodo que se llaman mutuamente
(test (run-val '(local
              [(define o (object
                          (method even (val) (if (= val 0)
                                                 #t
                                                 (if (= val 1)
                                                     #f
                                                     (send this
                                                           odd
                                                           (- val 1)))))
                          (method odd (val) (if (= val 1)
                                                 #t
                                                 (if (= val 0)
                                                     #f
                                                     (send this
                                                           even
                                                           (- val 1)))))
                          ))]
                  (send o even 10)))
      #t)

;Prueba de metodo recursivo
(test (run-val '(local
              [(define o (object
                          (method fib (val) (if (= val 0)
                                                 1
                                                 (if (= val 1)
                                                     1
                                                     (+ (send this
                                                              fib (- val
                                                                     1))
                                                        (send this
                                                              fib (- val
                                                                     2))))))
                          ))]
                  (send o fib 6)))
      13)

;Prueba con objeto sin define
(test (run-val '(send (object
                          (field x 1)
                          (field y 2)
                          (method get-x () (get x)))
                      get-x))
      1)

;Prueba de varios objetos
(test (run-val '(local
              [(define o1 (object
                          (field x 2)
                          (method double (val) (* val
                                                  (get x)))))]
            (+ (send o1 double 5)
               (send (object
                          (field x 4)
                          (method double (val) (* val
                                                  (get x))))
                     double 25))))
      110)


;Primer ejemplo de enunciado: comportamiento esperado
(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get x) (+ (get y) z)))
                          (method set-x (val) (set x val))
                          (method get-y () (get y))))]
            (seqn
             (send o set-x (+ 1 3))
             (+ (send o sum 3) (send o get-y)))))
      11)

;Segundo ejemplo de enunciado: los objetos son valores
(test (run-val
       '(local
            [(define a
               (object
                (method auto-apply (o)
                        (send o apply o))
                (method foo () 5)))
             (define o (send a auto-apply
                             (object
                              (method apply (other) (send other apply2 this))
                              (method apply2 (other) this)
                              (method foo () 42))))]
          (send o foo)))
      42)

;Caso de acceso a un campo inexistente de un objeto con get
(test/exn (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method get-z () (get z))))]
            (send o get-z)))
          "field not found")

;Caso de acceso a un campo inexistente de un objeto con set
(test/exn (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method set-z (val) (set z val))))]
            (send o set-z 10)))
          "field not found")

;Caso de invocacion de un metodo inexistente
(test/exn (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method get-x () (get x))))]
            (send o get-z)))
          "error: method")

;Caso de uso de get fuera de un objeto
(test/exn (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method get-x () (get x))))]
            (get x)))
          "get used outside of an object")

;Caso de uso de set fuera de un objeto
(test/exn (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method get-x () (get x))))]
            (set x 20)))
          "set used outside of an object")

;Caso de uso de this fuera de un objeto
(test/exn (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method get-x () (get x))))]
            (send this get-x)))
          "this used outside of an object")

; Ejemplo de binding
(test (run-val '(local
                  [(define seller (object
                                   (method price (prod)
                                           (* (if (= prod 1)
                                                  (send this price1)
                                                  (if (= prod 2)
                                                      (send this price2)
                                                      0))
                                              (send this unit)))
                                   (method price1 () 100)
                                   (method price2 () 200)
                                   (method unit () 1)
                                   ))
                   (define broker (object : seller
                                          (method unit () 2)))]
                  (send seller price 1)
                  ))
      100)

(test (run-val '(local
                  [(define seller (object
                                   (method price (prod)
                                           (* (if (= prod 1)
                                                  (send this price1)
                                                  (if (= prod 2)
                                                      (send this price2)
                                                      0))
                                              (send this unit)))
                                   (method price1 () 100)
                                   (method price2 () 200)
                                   (method unit () 1)
                                   ))
                   (define broker (object : seller
                                          (method unit () 2)))]
                  (send broker price 1)
                  ))
      200)

;Ejemplos de singleton y objetos excepcionales
(test (run-val '(local
                  [(define true (object
                                 (method ifTrueFalse (t f) t)))
                   (define false (object
                                 (method ifTrueFalse (t f) f)))
                   (define light (object
                                  (field on false)
                                  (method turn-on () (set on true))
                                  (method turn-of () (set on false))
                                  (method on? () (get on))))
                   ]
                  (send (send light on?) ifTrueFalse 1 0)
                  ))
      0)

(test (run-val '(local
                  [(define true (object
                                 (method ifTrueFalse (t f) t)))
                   (define false (object
                                 (method ifTrueFalse (t f) f)))
                   (define light (object
                                  (field on false)
                                  (method turn-on () (set on true))
                                  (method turn-of () (set on false))
                                  (method on? () (get on))))
                   ]
                  (seqn (send light turn-on)
                        (send (send light on?) ifTrueFalse 1 0))
                  ))
      1)

; Ejemplos dobre info compartida a traves de delegacion
(test (run-val '(local
                  [(define point (object
                                  (method above (p2)
                                          (if (< (send this y?)
                                                 (send p2 y?))
                                              p2
                                              this))
                                  (method add (p2)
                                          (send factory
                                                make (+ (send this x?)
                                                        (send p2 x?))
                                                (+ (send this y?)
                                                   (send p2 y?))))
                                  ))
                   (define factory
                     (object
                      (method make (x-init y-init)
                              (object : point
                                      (field x x-init)
                                      (field y y-init)
                                      (method x? () (get x))
                                      (method y? () (get y))
                                      ))
                      ))
                   (define big-point
                     (object : point
                             (method x? () 1000000)
                             (method y? () (send this x?))))
                   (define p1 (send factory make 1 2))
                   (define p2 (send big-point add p1))]
                  (send (send p2 above p1) x?)
                  ))
      1000001)

(test (run-val '(local
                  [(define point (object
                                  ))
                   (define 1D-point
                     (object : point
                             (field x 5)
                             (method x? () (get x))
                             (method x! (nx) (set x nx))))
                   (define factory
                     (object
                      (method make-shared (y-init)
                              (object : 1D-point
                                      (field y y-init)
                                      (method y? () (get y))
                                      (method y! (ny)(set y ny))
                                      ))
                      ))
                   (define p1 (send factory make-shared 2))
                   (define p2 (send factory make-shared 4))]
                  (+ (send p1 x?)
                     (send p2 x?))
                  ))
      10)

(test (run-val '(local
                  [(define point (object
                                  ))
                   (define 1D-point
                     (object : point
                             (field x 5)
                             (method x? () (get x))
                             (method x! (nx) (set x nx))))
                   (define factory
                     (object
                      (method make-shared (y-init)
                              (object : 1D-point
                                      (field y y-init)
                                      (method y? () (get y))
                                      (method y! (ny)(set y ny))
                                      ))
                      ))
                   (define p1 (send factory make-shared 2))
                   (define p2 (send factory make-shared 4))]
                  (seqn (send 1D-point x! 10)
                        (+ (send p1 x?)
                           (send p2 x?)))
                  ))
      20)

;Pruebas de scope lexico y delegacion
(test (run-val '(local
                  [(define parent
                    (object
                     (method foo () 1)))
                   (define outer
                     (object
                      (field foo 2)
                      (method foo () 3)
                      (method get ()
                              (object : parent
                                      (method get-foo1 () (get foo))
                                      (method get-foo2 () (send this foo))
                                      ))
                      ))
                   (define inner (send outer get))]
                  (send inner get-foo1)
                  ))
      2)

(test (run-val '(local
                  [(define parent
                    (object
                     (method foo () 1)))
                   (define outer
                     (object
                      (field foo 2)
                      (method foo () 3)
                      (method get ()
                              (object : parent
                                      (method get-foo1 () (get foo))
                                      (method get-foo2 () (send this foo))
                                      ))
                      ))
                   (define inner (send outer get))]
                  (send inner get-foo2)
                  ))
      1)

(test (run-val '(local
                    ([define o1 (object
                                (field f 1)
                                (method set-f (x) (set f x)))]
                     [define o2 (object : o1
                                       (field f 10)
                                       (method get-f () (get f)))])
                  (seqn (send o2 set-f (+ 10 (send o2 get-f)))
                  (send o2 get-f))))
      10)

(test/exn (run-val '(local
                    ([define o1 (object
                                (field f 1)
                                (method set-f (x) (set f x)))]
                     [define o2 (object : o1
                                       (field f 10)
                                       (method get-f () (get f)))])
                  (seqn (send o2 set-f (+ 10 (send o1 get-f)))
                  (send o2 get-f))))
          "error: method")

(test (run-val '(local
                  [(define point (object
                                  (field x 10)
                                  (method x? () (get x))))
                   (define point2 (object : point
                       (field x 20)))]
                  (send point2 x?)))
      10)

;Tercer ejemplo de enunciado: delegacion simple
(test (run-val '(local
              [(define smart-computer (object
                                       (method secret? (something) 42)))
               (define everything (object))
               (define oracle (object : smart-computer))]
               (send oracle secret? everything)))
      42)

;Cuarto ejemplo de enunciado: los objetos son valores
(test (run-val '(local
              [(define seller (object
                               (method multiplier () 1)
                               (method price (item-number)
                                       (* item-number (send this multiplier)))))
               (define broker (object : seller
                                      (method multiplier () 2)))]
               (send broker price 3)))
      6)

;Quinto ejemplo de enunciado: 
(test (run-val '(local
               ([define x (object
                           (field z 3)
                           (method get () (get z)))]
                [define y (object : x)])
             (send y get)))
      3)

;Sexto ejemplo de enunciado:
(test/exn (run-val '(local
               ([define x (object
                           (field z 3)
                           (method get () (get z)))]
                [define y (object : x
                               (method get () (get z)))])
             (send y get)))
          "field not found")

;Septimo ejemplo de enunciado:
(test (run-val '(local
	([define x (object
				(field f 3)
				(method f-getter () (get f))
				(method get () (send z msg)))]
	[define y (object : x
				(field f 5)
				(method msg () (send this f-getter))
				(method f-getter () (get f)))]
	[define z (object : y
				(field f 8)
				(method f-getter () (get f)))])
				(send y get)))
      8)

; Ejemplos de enunciado para probar shallow y deep copy

; a simple monotone counter
(define counter '(object
                  (field count 0)
                  (method incr () (set count (+ 1 (get count))))
                  (method get () (get count))))

; sequence of operations over 2 counters
(define (incrs-multiply x y)
  `(seqn
    (send ,y incr)
    (seqn
     (send ,x incr)
     (seqn
      (send ,x incr)
      (* (send ,x get) (send ,y get))
      ))))

;; no delegation, shallow-copy -> no sharing     
(test (run-val
       `(local ([define c ,counter])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

;; delegation, shallow-copy -> sharing
(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      16)

;; delegation, deep-copy -> no sharing
(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (deep-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

;Ejemplos para comprobar el uso de lambdas para el BONUS
(test (run-val '((fun () (+ 2 3))))
      5)

(test (run-val '((fun (x) (+ x x)) 3))
      6)

(test (run-val '((fun (x y) (* y x)) 10 20))
      200)

(test (run-val '(local
              [(define f (fun (x)
                              (+ x x)))]
              (f 5)))
      10)

(test (run-val '(local
              [(define test (fun (x y z w)
                              (+ (+ (* x 1000)
                                    (* y 100))
                                 (+ (* z 10)
                                    (* w 1)))))]
              (test 1 2 3 4)))
      1234)

(test (run-val '(local
              [(define test (fun (x y)
                                  (* x y)))
               (define test2 (fun (x)
                              (+ x (test x x))))
               ]
              (test2 5)))
      30)

(test (run-val '(local
              [(define fib (fun (x)
                                (if (= 0 x)
                                    0
                                    (if (= 1 x)
                                        1
                                        (+ (fib (- x 1))
                                           (fib (- x 2)))))))
               ]
              (fib 10)))
      55)
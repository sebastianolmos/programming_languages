#lang play
#|
 Gramática BNF del lenguaje

<prog>   ::= {<fundef>* <expr>}
 
<fundef> ::= {define {<id> <arg>*} [: <type>] <expr>}

<arg>    ::= <id> | {<id> : <type>}       
           | {<id> [: <type>] @ <contract>}

<type>   ::= Num | Bool| Any

<expr> ::= <num>
         | <id>
         | <bool>  
         | {<unop> <expr>}
         | {<binop> <expr> <expr>}
         | {if <expr> <expr> <expr>}
         | {with { {<id> [: <type>] <expr>}* } <expr>}
         | {<id> <expr>*} 

<id> ::= <symbol?>
<unop>   ::= ! | add1 | sub1   
<binop>  ::= + | - | * | / | && | || | == | > | <
|#

; Definicion de una expresion
(deftype Expr
  [num n]
  [id name]
  [bool b]
  {num-unop op arg}
  {bool-unop op arg}
  (num-binop op l r)
  (bool-binop op l r)
  (num-bool-binop op l r)
  (if-op sent t-case f-case)
  [with name-expr-list body-expr]
  [app fname arg-exprs])

; parse-unop :: Sym -> Op
; Entrega un operador unario
(define (parse-unop un)
  (match un
    ['add1 (lambda (x) (+ x 1))]
    ['sub1 (lambda (x) (- x 1))]
    ['! (lambda (x) (not x))]))

; Lista con los simbolos de operadores unarios de parametro num
(define num-unops (list 'add1 'sub1))

; is-num-unop? :: Sym -> Boolean
; Verifica si un simbolo representa un operador unario de parametro num
(define (is-num-unop? x) (member x num-unops))

; Lista con los simbolos de operadores unarios de parametro bool
(define bool-unops (list '!))

; is-bool-unop? :: Sym -> Boolean
; Verifica si un simbolo representa un operador unario de parametro bool
(define (is-bool-unop? x) (member x bool-unops))

; parse-binop :: Sym -> Op
; Entrega un operador binario
(define (parse-binop bin)
  (match bin
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    ['&& (lambda (x y) (and x y))]
    ['|| (lambda (x y) (or x y))]
    ['== (lambda (x y) (eq? x y))]
    ['< (lambda (x y) (< x y))]
    ['> (lambda (x y) (> x y))]))

; Lista con los simbolos de operadores binarios de parametros num
(define num-binops (list '+ '- '* '/))

; is-num-binop? :: Sym -> Boolean
; Verifica si un simbolo representa un operador binario de parametros num
(define (is-num-binop? x) (member x num-binops))

; Lista con los simbolos de operadores binarios de parametros bool
(define bool-binops (list '&& '||))

; is-bool-binop? :: Sym -> Boolean
; Verifica si un simbolo representa un operador binario de parametros bool
(define (is-bool-binop? x) (member x bool-binops))

; Lista con los simbolos de operadores binarios de parametros num que entrega bool
(define num-bool-binops (list '== '> '<))

; is-num-bool-binop? :: Sym -> Boolean
; Verifica si un simbolo representa un operador binario de parametros num que entrega bool
(define (is-num-bool-binop? x) (member x num-bool-binops))

; parse :: S-expr -> Expr
; Parsea la sintaxis concreta entregada
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(? boolean?) (bool s-expr)]
    [(list (? is-num-unop? op) arg) (num-unop (parse-unop op) (parse arg))]
    [(list (? is-bool-unop? op) arg) (bool-unop (parse-unop op) (parse arg))]
    [(list (? is-num-binop? op) l r) (num-binop (parse-binop op) (parse l) (parse r))]
    [(list (? is-bool-binop? op) l r) (bool-binop (parse-binop op) (parse l) (parse r))]
    [(list (? is-num-bool-binop? op) l r) (num-bool-binop (parse-binop op) (parse l) (parse r))]
    [(list 'if c t f) (if-op (parse c) (parse t) (parse f))]
    [(list 'with ne-list b)
     (with (map
            (lambda (name-expr)
              (match name-expr
                [(list id ': type src)(list id (parse src) (parse-type type))]
                [(list id src)(list id (parse src) (Any))]))
            ne-list) (parse b))]
    [(cons fname args) (app fname
                         (map parse args))]))

; Definicion de un tipo, donde Any es el tipo comodin
(deftype Type
  [Num]
  [Bool]
  [Any])

; parse-type :: Sym -> Type
; Entrega el typo que represnta un simbolo
(define (parse-type type)
  (match type
    ['Num (Num)]
    ['Bool (Bool)]
    ['Any (Any)]))

#|
representacion BNF:
<typenv> ::= (mtTypenv)
           | (aTypenv <id> <type> <typenv>)
|#

; Definicion de ambiente para tipos: Typenv
(deftype Typenv
  (mtTypenv)
  (aTypenv id type typenv))

; empty-typenv :: Typenv
; Crea un ambiene de tipos vacio
(define empty-typenv (mtTypenv))

; extend-typenv :: Sym Type Typenv -> Typenv
; Añade un simbolo con su tipo al ambiente de tipos
(define extend-typenv aTypenv)

; typenv-lookup :: Sym Env -> Type (o error)
; Busca el tipo de un simbolo en el ambiente de tipos
(define (typenv-lookup x typenv)
  (match typenv
    [(mtTypenv) (error 'typenv-lookup "Static error: free identifier: ~a" x)]
    [(aTypenv id type rest)
     (if (eq? id x)
         type
         (typenv-lookup x rest))]))

; extend-list-typenv :: Listof(Listof(Sym Expr Type)) Typenv TypeFunDef -> Typenv (o error)
; Genera una ambiente de tipos a aprtir de la lista de argumentos entregados por un with
; Realiza la verificacion de tipos estaticas para los argumentos del with
(define (extend-list-typenv name-expr-type-list typenv t-fundefs)
  (match name-expr-type-list
    ['() typenv]
    [(cons name-expr-type r-list)
     (let ([n-id (first name-expr-type)]
           [t-expr (typeof (second name-expr-type) typenv t-fundefs)]
           [t-dcl (third name-expr-type)])
       (if (Any? t-dcl)
           (extend-typenv n-id
                          (Any)
                          (extend-list-typenv r-list typenv t-fundefs))
           (if (Num? t-dcl)
               (if (or (Num? t-expr) (Any? t-expr))
                   (extend-typenv n-id
                                  (Num)
                                  (extend-list-typenv r-list typenv t-fundefs))
                   (error "Static type error: expected Num found Bool"))
               (if (or (Bool? t-expr) (Any? t-expr))
                   (extend-typenv n-id
                                  (Bool)
                                  (extend-list-typenv r-list typenv t-fundefs))
                   (error "Static type error: expected Bool found Num")))))]))

; fold-typenv :: Listof({Sym Type Sym}*) Typenv -> Typenv
; Genera un ambiente de tipos de una lista que contiene tuplas de nombres y tipos
(define (fold-typenv arg-types typenv)
  (match arg-types
    ['() typenv]
    [(list id type contract rest ...) (extend-typenv id
                                                     type
                                                     (fold-typenv rest typenv))]))

; perfect-t-fundef :: FunDef -> TypeFunDef
; Crea un fundef de tipos perfecto a partir de un fundef
; este typefundef perfecte se hace asumiendo que las funciones no tiene errores estaticos
(define (perfect-t-fundef fund)
  (let ([ftype (fundef-type fund)])
    (type-fundef (fundef-fname-args fund) ftype)))

; typeof-fundef :: FunDef Listof(TypeFunDef) -> TypeFunDef (o error)
; Genera un fundef de tipos a a partir de otro fundef
; Realiza la verificacion de tipos estatica con los tipos que se encuentran en un fundef
(define (typeof-fundef fund t-fundefs)
  (let ([ftypenv (fold-typenv (cdr (fundef-fname-args fund))
                              empty-typenv)]
        [ftype (fundef-type fund)]
        [fexpr (fundef-body fund)])
    (let ([btype (typeof fexpr ftypenv t-fundefs)])
      (if (Any? ftype)
          (type-fundef (fundef-fname-args fund) (Any))
          (if (Num? ftype)
              (if (or (Num? btype) (Any? btype))
                  (type-fundef (fundef-fname-args fund) (Num))
                  (error "Static type error: expected Num found Bool"))
              (if (or (Bool? btype) (Any? btype))
                  (type-fundef (fundef-fname-args fund) (Bool))
                  (error "Static type error: expected Bool found Num")))))))

; map-typeof :: Listof(Expr) Listof(TypeFunDef) Typenv -> Listof(Type)
; Calcula el tipo para cada expresion de la lista
(define (map-typeof expr-list t-fundefs typenv)
  (map
   (lambda (x) (typeof x typenv t-fundefs))
   expr-list))

; typeof :: Expr Typenv Listof(TypeFunDef) -> Type (o error)
; Calcula el tipo de una expresion o sintaxis abstracta y verifica estaticamente
(define (typeof expr typenv t-fundefs)
  (match expr
    [(num n) (Num)]
    [(id x) (typenv-lookup x typenv)]
    [(bool b) (Bool)]
    [(num-unop op arg) (let ([t (typeof arg typenv t-fundefs)])
                         (if (or (Num? t) (Any? t))
                             (Num)
                             (error "Static type error: expected Num found Bool")))]
    [(bool-unop op arg) (let ([t (typeof arg typenv t-fundefs)])
                         (if (or (Bool? t) (Any? t))
                             (Bool)
                             (error "Static type error: expected Bool found Num")))]
    [(num-binop op l r) (let ([tl (typeof l typenv t-fundefs)]
                              [tr (typeof r typenv t-fundefs)])
                         (if (and (or (Num? tl) (Any? tl))
                                  (or (Num? tr) (Any? tr)))
                             (Num)
                             (error "Static type error: expected Num found Bool")))]
    [(bool-binop op l r) (let ([tl (typeof l typenv t-fundefs)]
                               [tr (typeof r typenv t-fundefs)])
                          (if (and (or (Bool? tl) (Any? tl))
                                   (or (Bool? tr) (Any? tr)))
                              (Bool)
                              (error "Static type error: expected Bool found Num")))]
    [(num-bool-binop op l r) (let ([tl (typeof l typenv t-fundefs)]
                                   [tr (typeof r typenv t-fundefs)])
                               (if (and (or (Num? tl) (Any? tl))
                                        (or (Num? tr) (Any? tr)))
                                   (Bool)
                                   (error "Static type error: expected Num found Bool")))]
    [(if-op sent t-case f-case)
     (let ([ts (typeof sent typenv t-fundefs)]
           [tt (typeof t-case typenv t-fundefs)]
           [tf (typeof f-case typenv t-fundefs)])
       (if (Num? ts)
           (error "Static type error: expected Bool found Num")
           (if (or (Any? tt) (Any? tf))
               (Any)
               (if (Num? tt)
                   (if (Num? tf)
                       (Num)
                       (error "Static type error: expected Num found Bool"))
                   (if (Bool? tf)
                       (Bool)
                       (error "Static type error: expected Bool found Num"))))))]
    [(with name-expr-type-list bound-body)
     (typeof bound-body
             (extend-list-typenv name-expr-type-list typenv t-fundefs) t-fundefs)]
    [(app f arg-expr)
     (def (type-fundef n-args-types type) (lookup-type-fundef f t-fundefs))
     (let ([typed-args (map-typeof arg-expr t-fundefs typenv)])
       (let ([check (typecheck-args (cdr n-args-types) typed-args)])
         (if check
             type
             (error "error D:"))))]))

; print-type :: Type -> Sym
; Entrega el simbolo correspondiente a un tipo
(define (print-type type)
  (match type
    [(? Num?) 'Num]
    [(? Bool?) 'Bool]
    [(? Any?) 'Any]))
    

;; definicion de funcion
;; <fundef> := {fundef {<id> {<id> <type> <id>}*} <expr> <type>}
(deftype FunDef
  (fundef fname-args body type))

; lookup-fundef: Sym Listof(FunDef) -> FunDef (o error)
; Busca la definicion de una funcion o fundef en una lista de fundef
(define (lookup-fundef fname fundefs)
  (match fundefs
    ['() (error 'lookup-fundef "undefined function error: ~a" fname)]
    [(cons fd fds) (if (eq? (car (fundef-fname-args fd)) fname)
                       fd
                       (lookup-fundef fname fds))]))

;; definicion de funcion tipada
;; En vez de tener una expresion del body, solo guarda el tipo que retorna la funcion
;; <typefundef> := {typefundef {<id> {<id> <type> <id>}*} <type>}
(deftype TypeFunDef
  (type-fundef n-args-types type))

; lookup-type-fundef: Sym Listof(TypeFunDef) -> TypeFunDef (o error)
; Busca la definicion de una funcion para calcular tipo o typefundef en una lista de typefundef
(define (lookup-type-fundef fname t-fundefs)
  (match t-fundefs
    ['() (error  'lookup-type-fundef "Static contract error: ~a not defined" fname)]
    [(cons fd fds) (if (eq? (car (type-fundef-n-args-types fd)) fname)
                       fd
                       (lookup-type-fundef fname fds))]))

; typecheck-args :: Listof({Sym Type Sym}*) Listof(Type) -> Boolean (o error)
; Verifica que los tipos de los valores entregados a la funcion coincidan
; con los tipos declarados en los parametros de la funcion
(define (typecheck-args args-types expr-types)
  (match args-types
    ['() (if (empty? expr-types)
             #t
             (error "Static type error: vals dont match with args"))]
    [(list id ex-type contract rest ...)
     (if (empty? expr-types)
         (error "Static type error: args dont match with vals")
         (let ([r-type (car expr-types)])
           (if (Any? ex-type)
               (typecheck-args rest (cdr expr-types))
               (if (Num? ex-type)
                   (if (or (Num? r-type) (Any? r-type))
                       (typecheck-args rest (cdr expr-types))
                       (error
                        "Static type error: expected Num found Bool"))
                   (if (or (Bool? r-type) (Any? r-type))
                       (typecheck-args rest (cdr expr-types))
                       (error
                        "Static type error: expected Bool found Num")
                       )))))]))

; parse-fun-args :: Listof(Sym) -> Listof({Sym Type Sym}*)
; Parsea los argumentos de una funcion en sitaxis concreta (despues del define)
; Genera una lista con las tuplas de los argumentados parseados de una funcion
(define (parse-fun-args arg-list)
  (match arg-list
    ['() '()]
    [(list (list id ': type '@ contract) rest ...) (append (list id
                                                                 (parse-type type)
                                                                 contract)
                                                           (parse-fun-args rest))]
    [(list (list id ': type) rest ...) (append (list id
                                                     (parse-type type)
                                                     '())
                                               (parse-fun-args rest))]
    [(list (list id '@ contract) rest ...) (append (list id
                                                         (Any)
                                                         contract)
                                                   (parse-fun-args rest))]
    [(list id rest ...) (append (list id
                                      (Any)
                                      '())
                                (parse-fun-args rest))]))

; parse-fundef :: S-expr -> FunDef
; Parsea una lista que contiene la definicion de una funcion
(define (parse-fundef s-expr)
  (match s-expr
    [(list 'define fname-args ': type sexpr) (fundef (append (list (car fname-args))
                                                             (parse-fun-args (cdr fname-args)))
                                                     (parse sexpr)
                                                     (parse-type type))]
    [(list 'define fname-args sexpr) (fundef (append (list (car fname-args))
                                                     (parse-fun-args (cdr fname-args)))
                                             (parse sexpr)
                                             (Any))]))

#|
representacion BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

; Definicion de ambiente: Env
(deftype Env
  (mtEnv)
  (aEnv id val env))

; empty-env :: Env
; Genera un ambiente vacio
(define empty-env  (mtEnv))

; extend-env :: Sym Val Env -> Env
; Añade una variable y su valor a un ambiente dado
(define extend-env aEnv)

; env-lookup :: Sym Env -> Val (o error)
; Busca una variable en el ambiente y entrega su valor, si no es un error
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier error: ~a" x)]
    [(aEnv id val rest)
     (if (eq? id x)
         val
         (env-lookup x rest))]))

; fold-env :: Listof({Sym Type Sym}*) Listof(Val) -> Env (o error)
; Genera un ambiente agregando los parametros de una funcion junto con sus valores
; Tambien se veridica si la funcion tenia algun parametro bajo un CONTRATO, si es
; asi, busca el contrato para evaluar su funcion y verificar que lo cumpla
(define (fold-env arg-list val-list env fundefs)
  (match arg-list
    ['() (if (empty? val-list)
             env
             (error "error: vals dont match with args" ))]
    [(list f-arg f-type f-contract rest ...) (if (empty? val-list)
                             (error "error: args dont match with vals  ~a")
                             (if (eq? f-contract '())
                                 (extend-env f-arg
                                             (car val-list)
                                             (fold-env rest (cdr val-list) env fundefs))
                                 (let ([fund (lookup-fundef f-contract fundefs)])
                                   (let ([fname-args (fundef-fname-args fund)]
                                         [fbody (fundef-body fund)])
                                     (let ([result (interp (app f-contract (list (parse (car val-list))))
                                                           fundefs
                                                           empty-env)])
                                       (if (not result)
                                           (error 'fold-env
                                                  "Runtime contract error: ~a does not satisfy ~a"
                                                  (car val-list) f-contract)
                                           (extend-env f-arg
                                                       (car val-list)
                                                       (fold-env rest (cdr val-list) env fundefs))))))))]))
                                       
; extend-list-env :: Listof(Listof(Sym Expr)) FunDef Env -> Env
; Genera un ambiente a partir de una lista con pares {id expr} que entrega un with
(define (extend-list-env name-expr-list fundefs env)
  (match name-expr-list
    ['() env]
    [(cons f-name-expr r-list) (extend-env (car f-name-expr)
                                           (interp (cadr f-name-expr)
                                                   fundefs
                                                   env)
                                           (extend-list-env r-list fundefs env))]))

; bool-validation :: Val -> Boolean (o error)
; Verifica en Runtime si una expresion interpretada es un Boolean
(define (bool-validation exp)
  (match exp
    [(? number?) (error "Runtime type error: expected Boolean found Number" )]
    [(? boolean?) exp]))

; num-validation :: Val -> Num (o error)
; Verifica en Runtime si una expresion interpretada es un Num
(define (num-validation exp)
  (match exp
    [(? number?) exp]
    [(? boolean?) (error "Runtime type error: expected Number found Boolean")]))

; map-interp :: Listof(Expr) FunDef Env -> Listof(Val)
; Interpreta una lista de expresiones
(define (map-interp expr-list fundefs env)
  (map
   (lambda (x) (interp x fundefs env))
   expr-list))
    
; interp :: Expr Listof(FunDef) Env -> Val (o error)
; evalua una expresión en sintaxis abstracta
(define (interp expr fundefs env)
  (match expr
    [(num n) n]
    [(id x) (env-lookup x env)]  
    [(bool b) b]
    [(num-unop op arg) (op (num-validation(interp arg fundefs env)))]
    [(bool-unop op arg) (op (bool-validation(interp arg fundefs env)))]
    [(num-binop op l r) (op (num-validation(interp l fundefs env))
                            (num-validation(interp r fundefs env)))]
    [(bool-binop op l r) (op (bool-validation(interp l fundefs env))
                             (bool-validation(interp r fundefs env)))]
    [(num-bool-binop op l r) (op (num-validation(interp l fundefs env))
                                 (num-validation(interp r fundefs env)))]
    [(if-op sent t-case f-case)
     (if (bool-validation(interp sent fundefs env))
         (interp t-case fundefs env)
         (interp f-case fundefs env))]
    [(with name-expr-list bound-body)
     (interp bound-body
             fundefs
             (extend-list-env name-expr-list fundefs env))]
    [(app f arg-expr)
     (def (fundef fname-args fbody type) (lookup-fundef f fundefs))
     (interp fbody
             fundefs
             (fold-env (cdr fname-args)
                       (map-interp arg-expr fundefs env)
                       empty-env
                       fundefs))]))

;; Representacion de programa
;; <prog>   ::= {<fundef>* <expr>}
(deftype Prog
  (prog fundefs expr))

; search-contract :: Listof({Sym Type Sym}*) TypeFunDef -> Boolean (o error)
; Busca de manera estatica si un parametro de una funcion tiene un contrate
; Si encuentra un contrato, verifica de manera estatica los tipos
(define (search-contract args-types t-fundefs)
  (match args-types
    ['() #f]
    [(list id type contract rest ...) (if (eq? contract '())
                                          (search-contract rest t-fundefs)
                                          (let ([ct-fundef (lookup-type-fundef contract t-fundefs)])
                                            (if (eq? ct-fundef '())
                                                (search-contract rest t-fundefs)
                                                (let ([r-type (type-fundef-type ct-fundef)]
                                                      [arg-type (third
                                                                 (type-fundef-n-args-types ct-fundef))])
                                                  (if (not (and (Any? arg-type) (Bool? r-type)))
                                                      (error  'search-contract
                                                              "Static contract error: invalid type for ~a"
                                                              contract)
                                                      (search-contract rest t-fundefs))))))]))

; check-contract :: TypeFunDef Listof(TypeFunDef) -> Boolean (o error)
; Empieza el chequeo de contratos en un definicion tipada de funcion
(define (check-contract tf t-fundefs)
  (let ([args-types (cdr (type-fundef-n-args-types tf))])
    (search-contract args-types t-fundefs)))

; parse-program :: S-expr Listof(FunDef) -> Prog
; Parsea la sintaxis concreta de la declaracion de un programa, dejando
; sus partes definidas, funciones y main expr
(define (parse-program s-expr [fundefs '()])
  (match s-expr
    [(cons sexpr '()) (prog fundefs (parse sexpr))]
    [(cons fundef r-list) (parse-program r-list (append (list
                                                         (parse-fundef fundef))
                                                        fundefs))]))
; run :: S-expr -> Val (o error)
; Ejecuta un programa a partir de su sintaxis concreta, entregando su valor
; Realiza lo siguiente:
;       - Parsea la sintaxis concreta, entregando sus partes separadas en un Prog
;       - Crea typefundefs perfectos por cada funcion, esto es para cuando se hace la verificacion
;         estatica de las funciones, en el caso de una funcion llame a otra, se haga suponiendo que
;         las funciones estan bien declaradas. En el caso de estar mal, el programa arrojara error cuando
;         toque verificarla en su momento
;       - Realiza la verificacion estatica de tipos para cada funcion
;       - Realiza la verificacion estatica de contratos
;       - Realiza la verificacion estatica de tipos de la expresion del programa, calculando su tipo
;       - Finalmente interpreta la expresion (internamente se verifican dinamicamente los contratos)
(define (run s-expr)
  (def (prog fundefs expr) (parse-program s-expr)) ; Parsea el programa
  (let ([p-t-fundefs (map perfect-t-fundef fundefs)]) ; Crea typefundefs perfectos o ideales
    (let ([t-fundefs (map (lambda (x) (typeof-fundef x p-t-fundefs)) fundefs)])  ; Verificacion estatica de tipos para las funciones
      (let ([contracts (map (lambda (x) (check-contract x t-fundefs)) t-fundefs)]) ; Verificacion estatica de contratos
        (let ([prog-type (typeof expr empty-typenv t-fundefs)]) ; Verificacion estatica de tipos
          (interp expr fundefs empty-env)))))) ; Interpretacion

; typecheck :: S-expr -> Sym (o error)
; Ejecuta un programa a partir de su sintaxis concreta, entregando su valor
; Realiza lo siguiente:
;       - Parsea la sintaxis concreta, entregando sus partes separadas en un Prog
;       - Crea typefundefs perfectos por cada funcion, esto es para cuando se hace la verificacion
;         estatica de las funciones, en el caso de una funcion llame a otra, se haga suponiendo que
;         las funciones estan bien declaradas. En el caso de estar mal, el programa arrojara error cuando
;         toque verificarla en su momento
;       - Realiza la verificacion estatica de tipos para cada funcion
;       - Realiza la verificacion estatica de contratos
;       - Calcula el tipo de la expresion, realizando la verificacion estatica de esta
;       - Entrega el simbolo que representa el tipo
(define (typecheck s-expr)
  (def (prog fundefs expr) (parse-program s-expr)) ; Parsea el programa
  (let ([p-t-fundefs (map perfect-t-fundef fundefs)]) ; Crea typefundefs perfectos o ideales
    (let ([t-fundefs (map (lambda (x) (typeof-fundef x p-t-fundefs)) fundefs)]) ; Verificacion estatica de tipos para las funciones
      (let ([contracts (map (lambda (x) (check-contract x t-fundefs)) t-fundefs)]) ; Verificacion estatica de contratos
        (let ([prog-type (typeof expr empty-typenv t-fundefs)]) ; Verificacion estatica de tipos
          (print-type prog-type)))))) ; Entrega simbolo del tipo

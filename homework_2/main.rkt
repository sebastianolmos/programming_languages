#lang play

#|
<expr> ::= <num>
         | <bool>
         | <id>
         | <string>
         | {if <expr> <expr> <expr>}
         | {fun {<id>*}}  <expr>}
         | {<expr> <expr>*}
         | {local {<def>*} <expr>}
         | {match <expr> <case>+}

<case> ::= {'case <pattern> '=> <expr>}
<pattern> ::= <num>
         | <bool>
         | <string>
         | <id>
         | (<constr-id> <attr-id>*)

<def>  ::= {define <id> <expr>}
         | {datatype <typename> <type-constructor>*}}


<type-constructor> ::= {<id> <member>*}
<constr-id> :: = <id>
<attr-id> :: = <id>
<typename> :: = <id>
<member>   :: = <id>

|#
; expresiones
(deftype Expr
  (num n)
  (bool b)
  (str s)
  (ifc c t f)
  (id s)
  (app fun-expr arg-expr-list)
  (prim-app name args)   ; aplicación de primitivas
  (fun id body)
  (lcal defs body)
  (mtch val cases))

; definiciones
(deftype Def
  (dfine name val-expr) ; define
  (datatype name variants)) ; datatype

; variantes
(deftype Variant
  (variant name params))

; estructuras de datos
(deftype Struct
  (structV name variant values))

; estructura para almacenar las funciones generadas al interpretar un funciones
; en un define (en local), o las funciones que construyen estructuras StructV
(deftype Args
  (varArgs fun args)
  {funArgs body args})

; caso en pattern matching
(deftype Case
  (cse pattern body))

; patrón
(deftype Pattern
  (idP id) ; identificador
  (litP l) ; valor literal
  (constrP ctr patterns)) ; constructor y sub-patrones

;; parse :: s-expr -> Expr
(define(parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? boolean?) (bool s-expr)]
    [(? string?) (str s-expr)]
    [(? symbol?) (id s-expr)]
    [(list 'if c t f) (ifc (parse c) (parse t) (parse f))]
    [(list 'fun xs b) (fun xs (parse b))]
    [(list 'list args ...)(parse-list args)] ; Caso al parsear el azucar sintactico de la lista
    [(list 'with (list (list x e) ...) b)
     (app (fun x (parse b)) (map parse e))]
    [(list 'local defs body)
     (lcal (map parse-def defs) (parse body))]
    [(list 'match val-expr cases ...) ; note the elipsis to match n elements
     (mtch (parse val-expr) (map parse-case cases))] ; cases is a list
    [(list f args ...) ; same here
     (if (assq f *primitives*)
         (prim-app f (map parse args)) ; args is a list
         (app (parse f) (map parse args)))]))

;; parse-list :: s-expr -> Expr
;; Parsea el contenido da una lista usando el azucar sintactico
(define (parse-list s-expr)
  (match s-expr
    [(? empty?)(app (id 'Empty) '())]
    [(list head tail ...)(app (id 'Cons)
                              (list (parse head)
                                    (parse-list tail)))]))

; parse-def :: s-expr -> Def
(define(parse-def s-expr)
  (match s-expr
    [(list 'define id val-expr) (dfine id (parse val-expr))]
    [(list 'datatype name variants ...) (datatype name (map parse-variant variants))]))

; parse-variant :: sexpr -> Variant
(define(parse-variant v)
  (match v
    [(list name params ...) (variant name params)]))

; parse-case :: sexpr -> Case
(define(parse-case c)
  (match c
    [(list 'case pattern => body) (cse (parse-pattern pattern) (parse body))]))

; parse-pattern :: sexpr -> Pattern
(define(parse-pattern p)
  (match p
    [(? symbol?)  (idP p)]
    [(? number?)  (litP (num p))]
    [(? boolean?) (litP (bool p))]
    [(? string?)  (litP (str p))]
    [(list 'list args ...)(parse-list-pattern args)] ; Caso de parsear en un match el azucar sintactico de la lista
    [(list ctr patterns ...) (constrP (first p) (map parse-pattern patterns))]))

; parse-list-pattern :: sexpr -> Pattern
; parsea el azucar sintactico de una lista en posicion de case en un match
(define(parse-list-pattern p)
  (match p
    [(? empty?)(constrP 'Empty '())]
    [(list head tail ...)(constrP 'Cons
                                  (list {parse-pattern head}
                                        {parse-list-pattern tail}))]))

; s-apply :: Expr | Val -> Expr | Val
; Aplica un strict point, verificando si es un procedure para ejecutarlo,
; si es un structV, busca recurivamente si contiene algun procedure o si es
; otro tipo de valor lo devuelve
(define (s-apply val)
  (match val
    [(? procedure?) (apply val '())]
    [(structV name variant values)(structV name variant (map s-apply values))]
    [_ val]))

; s-apply :: Expr | Val -> Expr | Val
; Aplica un strict point en 1 capa, verificando si es un procedure para ejecutarlo,
;  o si es otro tipo de valor lo devuelve
(define (l-apply val)
  (match val
    [(? procedure?)(apply val '())]
    [_ val]))

; create-cache :: Expr Env -> Procedure
; Crea el cache necesario para tener call by need
(define (create-cache expr env)
  (let ([cache (box #f)])
    (λ ()
      (if (unbox cache)
          (begin
           ;(printf "using cached value ~v~n" (unbox cache)) 
           (unbox cache))
          (begin
            ;(printf "expresion in cache ~v~n" expr)
            (let ([res (l-apply (interp expr env))])
              ;(printf "init cache ~a~n" res)
              (set-box! cache res)
              res))))))

(define (check-id expr env)
  (match expr
    [(id x) (env-lookup x env)]
    [_ expr]))

; check-lazyness :: Expr Listof(Expr) Env -> Val
; Dado una expresion que corresponde a una funcion, chequea si tiene alguna declaracion
; de argumento como "lazy" y procede a cachear si es necesario, luego hace la
; interpretacion de la funcion con los argumentos cacheados si fue necesario
(define (check-lazyness fun-expr arg-list env)
  (match fun-expr  ; <-- Queremos revisar si una parametro de la funcion es lazy, para ello debemos consultar su definicion
    [(fun ids body)
     (let ([cached-list (map  ; <- Se crea una lista con los argumentos cacheados si son declarados lazy
                         (λ (id arg)
                           (match id
                             [(list 'lazy param)(create-cache arg env)]
                             [_ (interp arg env)]))
                         ids arg-list)]
           [id-list (map (λ (id) ; <- Se crea una lista con los nombre de parametros (se saca la declaracion de lazy)
                           (match id
                             [(list 'lazy param) param]
                             [_ id]))
                         ids)])
       (interp body (extend-env id-list cached-list env)))] ; <-  Al final se aplica la funcion directamente
    [_ (let ([fn (interp fun-expr env)]) ; <- La funcion esta en una id, procedemos:
         (match fn
           [(varArgs function params) ; Se detecta una funcion creadora de structuras o datatype
            (let ([cached-list (map  ; <-  Se crea una lista con los argumentos cacheados para cuando corresponda
                                (λ (id arg)
                                  (match id
                                    [(list 'lazy param)(if (procedure? arg)
                                                           (begin (arg)); se paso un valor ya cacheado
                                                           (begin (create-cache arg env)))]
                                    [_ (interp arg env)]))
                                params arg-list)])
              (function cached-list env))] ; <- Se aplica la funcion creadora con los argumentos cacheados si fue necesario
           [(funArgs body params) ; <- Se detecta una funcion declara como define dentro de un local
            (let ([cached-list (map  ; <- Se crea una lista con los argumentos cacheados si son declarados lazy
                                (λ (id arg)
                                  (match id
                                    [(list 'lazy param)(if (procedure? arg)
                                                           (begin (arg)); se paso un valor ya cacheado
                                                           (begin (create-cache arg env)))]
                                    [_ (interp arg env)]))
                                params arg-list)]
                   [id-list (map (λ (id) ; <- Se crea una lista con los nombre de parametros (se saca la declaracion de lazy)
                           (match id
                             [(list 'lazy param) param]
                             [_ id]))
                         params)])
              (interp body (extend-env id-list cached-list env)))] ; <-  Al final se aplica la funcion directamente
           [(? procedure?)(fn (map (λ (a) (interp a env)) arg-list))] ; <-  Si llega a encontrase un procedure, lo ejecuta
           ))]))


;; interp :: Expr Env -> number/boolean/procedure/Struct
(define(interp expr env)
  (match expr
    ; literals
    [(num n) n]
    [(bool b) b]
    [(str s) s]
    ; conditional
    [(ifc c t f)
     (if (l-apply (interp c env)) ;;<- poner strict points en el if
         (l-apply (interp t env))
         (l-apply (interp f env)))]
    ; identifier
    [(id x) (env-lookup x env)]
    ; function (notice the meta interpretation)
    [(fun ids body)
     (λ (arg-vals)
       (interp body (extend-env ids arg-vals env)))] 
    ; application
    [(app fun-expr arg-expr-list)
     (check-lazyness fun-expr arg-expr-list env)]  ; <- Hace el check de lazyness
    ; primitive application
    [(prim-app prim arg-expr-list)
     (apply (cadr (assq prim *primitives*))
            (map (λ (a) (s-apply (interp a env))) arg-expr-list))] ; <- Strict point
    ; local definitions
    [(lcal defs body)
     (def new-env (extend-env '() '() env))
     (for-each (λ (d) (interp-def d new-env)) defs)
     (interp body new-env)]
    ; pattern matching
    [(mtch expr cases)
     (def value-matched (interp expr env))
     (def (cons alist body) (find-first-matching-case value-matched cases))
     (s-apply (interp body (extend-env (map car alist) (map cdr alist) env)))]))  ; <- Strict point para el match

; interp-def :: Def Env -> Void
(define(interp-def d env)
  (match d
    [(dfine id val-expr)
     (update-env! id (match val-expr  ; <- Se detecta alguna definicion en un local
                       [(fun ids body)  ; <- Si es la definciion de una funcion hay que recordar que puede tener algun lazy
                        (funArgs body
                                 ids)]
                       [_ (interp val-expr env)]) ; <- Si no, se procede normalmente
                  env)]
    [(datatype name variants)
     ;; extend environment with new definitions corresponding to the datatype
     (interp-datatype name env)
     (for-each (λ (v) (interp-variant name v env)) variants)]))

; interp-datatype :: String Env -> Void
(define(interp-datatype name env)
  ; datatype predicate, eg. Nat?
  (update-env! (string->symbol (string-append (symbol->string name) "?"))
               (λ (v) (symbol=? (structV-name (first v)) name))
               env))

; interp-variant :: String String Env -> Void
(define(interp-variant name var env)
  ;; name of the variant or dataconstructor
  (def varname (variant-name var))
  ;; variant data constructor, eg. Zero, Succ
  (update-env! varname
               (varArgs (λ (args n-env) (structV name varname args))
                        (variant-params var))  ;; <- Aca se guarda la funcion constructora junto a la lista de params (que pueden tener lazyness)
               env)
  ;; variant predicate, eg. Zero?, Succ?
  (update-env! (string->symbol (string-append (symbol->string varname) "?"))
               (λ (v) (symbol=? (structV-variant (first v)) varname))
               env))

;;;;; pattern matcher
(define(find-first-matching-case value cases)
  (match cases
    [(list) #f]
    [(cons (cse pattern body) cs)
     (let [(r (match-pattern-with-value pattern value))]
       (if (foldl (λ (x y)(and x y)) #t r)
           (cons r body)
           (find-first-matching-case value cs)))]))

(define(match-pattern-with-value pattern value)
  (match/values (values pattern value)
                [((idP i) v) (list (cons i v))]
                [((litP (bool v)) b)
                 (if (equal? v b) (list) (list #f))]
                [((litP (num v)) n)
                 (if (equal? v n) (list) (list #f))]
                [((constrP ctr patterns) (structV _ ctr-name str-values))
                 (if (symbol=? ctr ctr-name)
                     (apply append (map match-pattern-with-value
                                        patterns str-values))
                     (list #f))]
                [((constrP ctr patterns) (? procedure?))  ; <- Si encuentra un procedura, lo ejecuta y vuelve a hacer la funcion
                 (match-pattern-with-value pattern (l-apply value))]
                [(x y)
                 (error "Match failure")]))




;; run :: s-expr -> number/boolean/procedura/struct
(define(run prog [flag ""])
  (def list-pars     ; <- Definicion de la estructura lista y de la funcion length
    (map parse-def '{{
                datatype List
                         {Empty}
                         {Cons a b}}
               {define length {fun {n}
                                   {match n
                                     {case {Empty} => 0}
                                     {case {Cons h t} =>
                                       {+ 1 {length t}}}}}}}))
  (def list-env (extend-env '() '() empty-env))  ; <- Crea un ambiente vacio para cargar la definicion de listas
  (for-each (λ (d) (interp-def d list-env)) list-pars)   ; <- Carga las listas en el ambiente
  (def val (l-apply (interp (parse prog) list-env)))  ; <- Parsea, Interpreta el programa con las listas, luego tiene un strict point
  (if (structV? val)  ; <- Realiza un print si el flag lo indica
      (match flag
        ["ppwu" (pretty-printing val)]
        ["pp" (pretty-list val)]
        [_ val])
      val))


#|-----------------------------
Environment abstract data type
empty-env   :: Env
env-lookup  :: Sym Env -> Val
extend-env  :: List[Sym] List[Val] Env -> Env
update-env! :: Sym Val Env -> Void
|#
(deftype Env
  (mtEnv)
  (aEnv bindings rest)) ; bindings is a list of pairs (id . val)

(def empty-env  (mtEnv))

(define(env-lookup id env)
  (match env
    [(mtEnv)
     (error 'env-lookup "no binding for identifier: ~a" id)]
    [(aEnv bindings rest)
     (def binding (assoc id bindings))
     (if binding
         (cdr binding)
         (env-lookup id rest))]))

(define (extend-env ids vals env)
  (aEnv (map cons ids vals) ; zip to get list of pairs (id . val)
        env))

;; imperative update of env, adding/overriding the binding for id.
(define(update-env! id val env)
  (set-aEnv-bindings! env (cons (cons id val) (aEnv-bindings env))))

;;; primitives
; http://pleiad.cl/teaching/primitivas

(define *primitives*
  `((+       ,(lambda args (apply + args)))
    (-       ,(lambda args (apply - args)))
    (*       ,(lambda args (apply * args)))
    (%       ,(lambda args (apply modulo args)))
    (odd?    ,(lambda args (apply odd? args)))
    (even?   ,(lambda args (apply even? args)))
    (/       ,(lambda args (if (equal? (second args) 0)
                               (error "division by zero")
                               (apply / args))))
    (=       ,(lambda args (apply = args)))
    (<       ,(lambda args (apply < args)))
    (<=      ,(lambda args (apply <= args)))
    (>       ,(lambda args (apply > args)))
    (>=      ,(lambda args (apply >= args)))
    (zero?   ,(lambda args (apply zero? args)))
    (not     ,(lambda args (apply not args)))
    (and     ,(lambda args (apply (lambda (x y) (and x y)) args)))
    (or      ,(lambda args (apply (lambda (x y) (or x y)) args)))))


; pretty-printing :: Val | StructV -> String
; Imprime el resultado de la interpretacion de manera que las estructuras se van de manera
; agradable para el usuario, realiza un strict si es necesario
(define (pretty-printing struct-val)
  (define {pp-empty value}
    (match value
      [(structV s-name s-variant s-values)(string-append
                                           "{"
                                           (~a s-variant)
                                           (foldr string-append ""
                                                  (map (lambda (x)
                                                         (string-append
                                                          " "
                                                          (pp-empty x)
                                                          )) s-values))
                                           "}"
                                           )]
      [(? empty?) ""]
      [(? number?) (~a value)]
      [(? boolean?) (~a value)]
      [(? string?) value]
      [(? symbol?) (~a value)]
      [(? procedure?) (pp-empty (apply value '()))]))
  (match struct-val
    [(structV s-name s-variant s-values) (pp-empty struct-val)]))

; pretty-printing :: Val | StructV -> String
; Imprime el resultado de la interpretacion de manera que las estructuras se van de manera
; agradable para el usuario y si hay listas, las imprime como tal, realiza un strict si es necesario
(define [pretty-list struct-val]
  (define (pp-list struct-list)
    (match struct-list
      [(structV 'List 'Empty '()) ""]
      [(structV 'List 'Cons values)(string-append
                                    " "
                                    (pp-empty (first values))
                                    (pp-list (second values)))]))
  (define {pp-empty value}
    (match value
      [(structV 'List 'Empty '()) "{list}"]
      [(structV 'List 'Cons values)(string-append
                                           "{list "
                                           (pp-empty (first values))
                                           (pp-list (second values))
                                           "}"
                                           )]
      [(structV s-name s-variant s-values)(string-append
                                           "{"
                                           (~a s-variant)
                                           (foldr string-append ""
                                                  (map (lambda (x)
                                                         (string-append
                                                          " "
                                                          (pp-empty x)
                                                          )) s-values))
                                           "}"
                                           )]
      [(? empty?) ""]
      [(? number?) (~a value)]
      [(? boolean?) (~a value)]
      [(? string?) value]
      [(? symbol?) (~a value)]
      [(? procedure?) (pp-empty (value))]))
  (match struct-val
    [(structV s-name s-variant s-values) (pp-empty struct-val)]))

; Definicion de un stream en MiniScheme+
(def stream-data '{datatype Stream
                            {Pair head {lazy tail}}})

; Definicion de una funcion creadora de streams en MiniScheme+
(def make-stream '{define make-stream
                    {fun {hd {lazy tl}}
                         {Pair hd tl}}})

; Definicion Stream infinito de 1s en MiniScheme+
(def ones '{define ones {make-stream 1 ones}})

; Definicion de una funcion que entrega la cabeza de un stream en MiniScheme+
(def stream-hd '{define stream-hd
                  {fun {stream}
                       {match stream
                         {case {Pair h t} => h}}}})

; Definicion de una funcion que entrega la cola de un stream en MiniScheme+
(def stream-tl '{define stream-tl
                  {fun {stream}
                       {match stream
                         {case {Pair h t} => t}}}})

; Definicion de una funcion que toma n cantidad de elementos de un stream en MiniScheme+
(def stream-take '{define stream-take
                    {fun {n stream}
                         {if (zero? n)
                             {Empty}
                             {Cons {stream-hd stream}
                                   {stream-take {- n 1}
                                                {stream-tl stream}}}}}})

; Definicion de funciones de MiniScheme+
(def stream-lib (list stream-data
                      make-stream
                      stream-hd
                      stream-tl
                      stream-take))

; Definicion de la funcion zipWith en MiniScheme+
; que aplica una funcion entre dos stream generando otro
(def stream-zipWith '{define stream-zipWith
                       {fun {function stream1 stream2}
                            {make-stream
                             {function {stream-hd stream1}
                                       {stream-hd stream2}}
                             {stream-zipWith function
                                             {stream-tl stream1}
                                             {stream-tl stream2}}}}})

; Definicion de un stream que representa la secuencia de fibonacci en MiniScheme+
(def fibs '{define fibs
             {make-stream 1 {make-stream 1
                          (stream-zipWith {fun {n m}
                                               {+ n m}}
                                          fibs
                                          {stream-tl fibs})}}})

; Definicion de una funcion que ordena los elementos de dos streams en MiniScheme+
(def merge-sort '{define merge-sort
                   {fun {{lazy stream1} {lazy stream2}}
                        {if {< {stream-hd stream1}
                               {stream-hd stream2}}
                            {make-stream {stream-hd stream1}
                                         {merge-sort {stream-tl stream1}
                                                     stream2}}
                            {make-stream {stream-hd stream2}
                                         {merge-sort {stream-tl stream2}
                                                     stream1}}}}})


#lang play

#|
<expr> ::= <num>
         | <id>
         | <bool>
         | (if <expr> <expr> <expr>)
         | (+ <expr> <expr>)
         | '< <s-expr> <s-expr>)
         | (* <s-expr> <s-expr>)
         | (= <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (and <s-expr> <s-expr>)
         | (or <s-expr> <s-expr>)
         | (not <s-expr> <s-expr>)
         | (
 <expr> <expr>)
         | (local { <def> ...} <expr>)

<def>    ::= (define <id> <expr>)


;EXTENSION PARA OBJETOS
<expr>  ::= ... (todo lo anterior)
         | (object [: <expr>] <member> ...)
         | this
         | (set <id> <expr>)
         | (get <id>)
         | (send <expr> <id> <expr> ...)
         | (shallow-copy <expr>)
         | (deep-copy <expr>)

<member> ::=
        | (field <id> <s-expr>)
        | (method <id> (list <id> ...) <s-expr>)

|#

(deftype Expr
  (num n)
  (bool b)
  (id s)
  (binop f l r)
  (unop f s)
  (my-if c tb fb)
  (seqn expr1 expr2)
  (lcal defs body)
  (object parent members)
  (get id)
  (set id expr)
  (this)
  (send o-expr m-id args)
  (shallow obj)
  (deep obj))

; Miembros de un objeto
(deftype Member
  (fieldM id expr)
  (methodM id params body))

;; values
(deftype Val
  (numV n)
  (boolV b)
  (objectV parent fields methods env)) ;<- Objeto como valor

;; Contexto que contiene en current al objeto actual en donde se busca los
;; miembros y en self el objeto al cual empezar a aplicar los metodos
(deftype Ctx
  (ctx current self))

#|-----------------------------
Method environment abstract data type
 
empty-menv  :: Menv
extend-menv :: MethodM Menv -> Menv
menv-lookup :: MethodM Env -> Val
menv-has?   :: MethodM Env -> Bool
 
representation BNF:
<menv> ::= (mtMenv)
        | (aMenv <method> <menv>)
|#
;Equivalente a un ambiente "normal" pero para metodos
(deftype Menv
  (mtMenv)
  (aMenv met menv))
 
(def empty-menv (mtMenv))
 
(def extend-menv aMenv)
 
(define (menv-lookup x menv)
  (match menv
    [(mtMenv) (error 'menv-lookup "error: method ~a not found" x)]
    [(aMenv met rest)
     (if (symbol=? (methodM-id met) x)
         met
         (menv-lookup x rest))]))

; Funcion para preguntar si un Menv contiene el metodo deseado
(define (menv-has? x menv)
  (match menv
    [(mtMenv) #f]
    [(aMenv met rest)
     (if (symbol=? (methodM-id met) x)
         #t
         (menv-has? x rest))]))


(deftype Def
  (my-def id expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment abstract data type

empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env))

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest) (hash-ref hash x (λ () (env-lookup x rest)))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-immutable-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))

#|
extend-frame-env!:: Sym Val Env -> Void
Agrega un nuevo par (Sym, Val) al ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (hash id val) env)]
    [(aEnv h rEnv) (def hupd (hash-set h id val))
                   (set-aEnv-hash! env hupd)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    ['this (this)]
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b))]
    [(list 'object ': p members ...)(object (parse p)
                                            (map parse-member members))]
    [(list 'object members ...)(object '() (map parse-member members))]
    [(list 'get id)(get id)]
    [(list 'set id e)(set id (parse e))]
    [(list 'send o m-id args ...)(send (parse o) m-id (map parse args))]
    [(list 'shallow-copy o)(shallow (parse o))]
    [(list 'deep-copy o)(deep (parse o))]
    [(list 'fun xs b) (object '() (list  ; <- Azucar sintactico para parsear definicion de lambdas
                                   (methodM 'fun
                                            xs
                                            (parse b))))]
    [(list f args ...)(send (parse f) 'fun (map parse args))]; <- Azucar sintactico para parsear aplicacion de lambdas
    ))

; Parse la sintaxis concreta de los miembros de un objeto
;; parse-member :: s-expr -> Expr
(define (parse-member m-expr)
  (match m-expr
    [(list 'field id e)(fieldM id (parse e))]
    [(list 'method id p-list e)(methodM id p-list (parse e))]))

;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))

;; Evalua con el Contexto que incluye en scope algun Objeto, si es necesario
;; interp :: Expr Env [Ctx]-> Val
(define (interp expr env [ctex '()])
  (match expr
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(binop f l r) (make-val (f (open-val (interp l env ctex))
                                (open-val (interp r env ctex))))]
    [(unop f s) (make-val (f (open-val (interp s env ctex))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env ctex))
     (if cnd
         (interp t env ctex)
         (interp f env ctex))]
    
    [(this) (interp-this ctex)]
    [(id x) (env-lookup x env)]
    [(seqn expr1 expr2) (begin
                          (interp expr1 env ctex)
                          (interp expr2 env ctex))]
    [(lcal defs body)
     (let ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (def (cons id val) (interp-def x new-env ctex))
                   (extend-frame-env! id val  new-env)
                   #t) defs)
       (interp body new-env ctex))
     ]
    [(object p members)(objectV
                        (interp-parent p env ctex)
                        (interp-fields members env ctex) ; Fields: Hash table de campos
                        (save-methods members) ; Methods: Env de metodos
                        env; Environment
                        )]
    [(get id)(interp-get id ctex)]
    [(set id expr)(interp-set id expr env ctex)]
    [(send o-id m args)(interp-method m
                                      (interp-obj o-id env ctex)
                                      (map (λ (arg)
                                             (interp arg env ctex))
                                           args)
                                      ctex)]
    [(shallow o)
     (def (objectV p fs ms e)(interp-obj o env ctex))
     (objectV p
              (hash-copy fs) ; <- Se copia la tabla de hash de los fields
              ms ; <- Por enunciado el ambiente de metodos y env no se copiaba
              e)]
    [(deep o)
     (def (objectV p fs ms e)(interp-obj o env ctex))
     (objectV (interp-deep p env ctex) ;<- Copia recursiva de los padres
              (hash-copy fs)
              ms
              e)]))

; Copia deep de un objeto padre, manteniendolo nulo si es necesario
; Y copiando cada padre con shallow copy
; interp-deep :: Expr|Null Env Ctx -> Val|Null
(define (interp-deep obj env ctex)
  (match obj
    ['() obj]
    [else (interp (shallow obj) env ctex)]))

; Evalua un objeto padre, manteniendolo nulo si es necesario
; interp-parent :: Expr|Null Env Ctx -> Val|Null
(define (interp-parent parent env ctex)
  (match parent
    ['() parent]
    [else (interp parent env ctex)]))

; Genera una Hash-table para guardar los campos y sus valores
; interp-fields :: Listof(Member) Env Ctx -> Dict(Sym Val)
(define (interp-fields members env obj)
  (match obj
    [(ctx c (objectV p f m e))
     (begin (def table (hash-copy f)) ;<- Copia los fields del Objeto en scope
            (for-each (λ (fm)
                        (dict-set! table
                                   (fieldM-id fm)
                                   (interp (fieldM-expr fm)
                                         env obj)))
                      (filter fieldM? members))
            table)]
    [else (make-hash
          (map (λ (m)
                 (cons (fieldM-id m) (interp (fieldM-expr m)
                                         env obj)))
               (filter fieldM? members)))]))

; Genera una lista con los metodos
; save-methods :: Listof(Member) -> Listof(MethodM)
(define (save-methods members)
  (def methods (filter methodM? members))
  (foldr (λ (newM menv)
           (extend-menv newM menv))
         empty-menv
         methods))

; Evalua la invocacion de un metodo considerando el objeto llamado sus delegaciones
; interp-method :: Sym ObjectV Listof(Val) Ctx -> Val (o error)
(define (interp-method m-id obj args ctex)
  (match obj
    [(objectV p f-list m-list env)
     (if (menv-has? m-id m-list)
         (match (menv-lookup m-id m-list); <- Metodo esta en el objeto actual
           [(methodM id p-list body)(interp body
                                            (multi-extend-env p-list
                                                              args
                                                              env)
                                            (if (null? ctex)
                                                (if (null? p)
                                                    (ctx '() obj)
                                                    (ctx obj obj))
                                                (if (null? (ctx-current ctex))
                                                    (ctx '() obj)
                                                    (ctx obj
                                                 (ctx-self ctex))))
                                            )])
         (if (null? p)
             (error 'interp-method "error: method ~a not found" m-id)
             (interp-method m-id p args
                            (ctx p obj)))); <- Se buscara el metodo en el padre
         ]))

; Busca un campo del objeto actual, arrojando error si no lo encuentra
; o no esta en un objeto
; interp-get :: Sym Ctx -> Val (o error)
(define (interp-get id obj)
  (match obj
    ['()(error "error: get used outside of an object")]
    [(ctx '() self)(if (dict-has-key? (objectV-fields self) id) ; Caso de objeto no delegado
                           (dict-ref (objectV-fields self) id)
                           (error "error: field not found"))]
    [(ctx current self)(if (dict-has-key? (objectV-fields current) id)
                           (dict-ref (objectV-fields current) id)
                           (error "error: field not found"))]
    ))

; Modifica un campo del objeto actual, arrojando error si no lo encuentra
; o no esta en un objeto
; interp-set :: Sym Expr Env Ctx -> Void (o error)
(define (interp-set id expr env obj)
  (match obj
    ['()(error "error: set used outside of an object")]
    [(ctx '() self)(if (dict-has-key? (objectV-fields self) id) ; Caso de objeto no delegado
                           (dict-set! (objectV-fields self)
                                      id
                                      (interp expr env obj))
                           (error "error: field not found"))]
    [(ctx current self)(if (dict-has-key? (objectV-fields current) id)
                           (dict-set! (objectV-fields current)
                                      id
                                      (interp expr env obj))
                           (error "error: field not found"))]
    ))

; Retorna el objeto actual, arrojando error si no esta en un objeto
; interp-this :: Ctx -> ObjectV (o error)
(define (interp-this obj)
  (match obj
    ['()(error "error: this used outside of an object")]
    [(ctx cr sf) sf]))

; Evalua un posible objeto, retornando si ya esta evaluado
; interp-this :: Expr Env Ctx -> ObjectV | Val
(define (interp-obj expr env obj)
  (match expr
    [(objectV p f m env) expr]
    [else (interp expr env obj)]))

;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    ))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;; interp-def :: Def, Env -> Expr
(define (interp-def a-def env obj)
  (match a-def
    [(my-def id body) (cons id (interp body env obj))]))

;; run :: s-expr -> Val
(define (run s-expr)
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define (run-val s-expr)
  (define val (interp (parse s-expr) empty-env))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [x x]))

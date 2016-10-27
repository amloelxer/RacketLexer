#lang racket

;;; Written by Alex Moller
;;; On 10/26/16

(require parser-tools/lex
         parser-tools/yacc)

(define-tokens v (VALUE
                  BINARYOP
                  UNARYOP
                  IF
                  THEN
                  ELSE
                  LET
                  IN
                  LAMBDA
                  CALL
                  WITH
                  ID))

(define-empty-tokens e (LEFTPAREN
                        RIGHTPAREN
                        EOF))

(define (lex-this lexer input)
  (lambda () (lexer input)))

(struct binaryOpExpression(firstArgument secondArgument) #:transparent)
(struct andExpression(firstArgument secondArgument)#:transparent)
(struct orExpression(firstArgument secondArgument)#:transparent)
(struct xorExpression(firstArgument secondArgument)#:transparent)
(struct unaryExpression(firstArgument) #:transparent)
(struct ifExpression(firstArgument secondArgument thirdArgument)#:transparent) 
(struct letExpression(firstArugment secondArgument thirdArgument)#:transparent)
(struct lambdaExpression(firstArgument secondArgument)#:transparent)

(struct callExpression(firstArgument secondArgument))

(struct valueExpression(firstArgument))

(struct idExpression(firstArgument))


; Implementation of Environment
;;; From Jeff
; empty-env : () -> Env
(define empty-env
  (lambda () (list 'empty-env)))

; extend-env : Var x SchemeVal x Env -> Env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))


;;; apply env from Jeff's Example
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (error "No Binding Found: ~s" search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (error "Invalid Environment: ~s"  env)))))
  
;;;true, false,(,), and, or, not, xor, lambda, let, in, if, then, else, call, with}
;;; Build Lamba Structures to make things easier
(define constantBoolLexer
  (lexer
    ["true" (token-VALUE lexeme)]
    ["false" (token-VALUE lexeme) ]
    [#\( (token-LEFTPAREN)]
    [#\) (token-RIGHTPAREN)]
    ;;And 
    [#\& (token-BINARYOP lexeme)]
    ;;Or 
    ["or" (token-BINARYOP lexeme)]
    ;;Not
    [#\! (token-UNARYOP lexeme)]
    ;;Xor
    [#\^ (token-BINARYOP lexeme)]
    ;;Lambda
    [#\λ (token-VALUE lexeme)]
    ;;Let
    ["let" (token-LET lexeme)]
    ;;In
    ["in" (token-IN lexeme) ]
    ;;if
    ["if" (token-IF lexeme)]
    ;;then
    ["then" (token-THEN lexeme)]
    ;;else
    ["else" (token-ELSE lexeme)]
    ;;call
    ["call" (token-CALL lexeme)]
    ;;with
    ["with" (token-WITH lexeme)]
    [(concatenation alphabetic (repetition 1 +inf.0 (union alphabetic numeric))) (token-ID lexeme)]
    [whitespace (constantBoolLexer input-port)]
    [(eof) (token-EOF)]
    ))

;;define grammar in exparser
(define expparser
  (parser
   (start exp)
   (end EOF)
   (tokens v e)
   (error void)
   (grammar
    (exp
     ;;not expression
     ;;Unary OP
     ((LEFTPAREN UNARYOP exp RIGHTPAREN) (unaryExpression $3))
     ;;VALUE
     ((VALUE) (valueExpression $1))
     ;::Binary OP
     ((LEFTPAREN exp BINARYOP exp RIGHTPAREN) (cond
                                                     [(equal? $3 "&") (andExpression $2 $4)]
                                                     [(equal? $3 "or") (orExpression $2 $4)]
                                                     [else (xorExpression $2 $4)]))

     ;;;Lambda
     ((LEFTPAREN LAMBDA LEFTPAREN ID RIGHTPAREN exp RIGHTPAREN) (lambdaExpression $4 $6))



     
     ;;;Let
     ((LEFTPAREN LET LEFTPAREN ID VALUE RIGHTPAREN IN exp RIGHTPAREN) (cond
                                                                        [(equal? $5 "λ") (lambdaExpression $4 $5 $8)]
                                                                        [(equal? $5 "&") (andExpression $4 $5 $8)]
                                                                        [(equal? $5 "|") (orExpression $4 $5 $8)]
                                                                        [(equal? $5 "^") (xorExpression $4 $5 $8)]
                                                                        [(equal? $5 "true") (valueExpression $5)]
                                                                        [(equal? $5 "false") (xorExpression $5)]
                                                                        [(equal? $5 "!") (unaryExpression $8)]))
                                                                       

     ;;; IF
     ((LEFTPAREN IF exp THEN exp ELSE exp RIGHTPAREN) (ifExpression $3 $5 $7))
     ;;; Call
     ((LEFTPAREN ID WITH exp RIGHTPAREN) (callExpression $2 $4))

     ;;IDExpression
     ((ID) (idExpression $1))
     
     )
    )
   )
  )

(define (evaluate aTree env)
  (match aTree
    ;;Unary
    [(unaryExpression argumentOne) (not (evaluate argumentOne empty-env))]
    ;;Value
    [(valueExpression argumentOne) (equal? argumentOne "true")]
    ;;Lambda
    [(lambdaExpression argumentOne argumentTwo) (lambda (val) (evaluate argumentTwo (extend-env argumentOne val env)))]
    ;;; ID
    [(idExpression argumentOne) (apply-env env argumentOne)]
    ;;;Let Expression
    [(letExpression argumentOne argumentTwo argumentThree) (evaluate argumentThree (extend-env argumentTwo argumentThree))]
    ;;; And
    [(andExpression argumentOne argumentTwo) (and (evaluate argumentOne empty-env) (evaluate argumentTwo empty-env))]
    ;;; Xor
     [(orExpression argumentOne argumentTwo) (or (evaluate argumentOne empty-env) (evaluate argumentTwo empty-env))]
    ;;; Xor
    [(xorExpression argumentOne argumentTwo) (xor (evaluate argumentOne empty-env) (evaluate argumentTwo empty-env))]
    ;;; IF
    [(ifExpression firstArgument secondArgument thirdArgument) (if (evaluate firstArgument empty-env) (evaluate secondArgument empty-env) (evaluate thirdArgument empty-env))]
    ;;; Call
    [(callExpression firstArgument secondArgument) (extend-env firstArgument evaluate(secondArgument env))]

))

;;;(evaluate (expparser (lex-this constantBoolLexer nottest)) (empty-env))
(define nottest (open-input-string "(! false)"))
(define andTest (open-input-string "(false & true)"))
(define orTest (open-input-string "(false or true)"))
(define xorTest (open-input-string "(false ^ true)"))
(define ifTest (open-input-string "(if true then true else false)"))

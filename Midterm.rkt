#lang racket

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
(struct andExpression(firstArgument secondArgument))
(struct orExpression(firstArgument secondArgument))
(struct xorExpression(firstArgument secondArgument))
(struct unaryExpression(firstArgument) #:transparent)
(struct ifExpression(firstArgument secondArgument thirdArgument)#:transparent) 

 ;;; Let a(first Expression) =(second expression) b (third expression)
(struct letExpression(firstArugment secondArgument ))
(struct lambdaExpression(firstArgument secondArgument))

;;Width is part of call
(struct callExpression(firstArgument secondArgument))

(struct valueExpression(firstArgument))

  
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
    ;;WHAT KIND OF TOKENS ARE THESE
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
     ((LEFTPAREN LAMBDA LEFTPAREN ID RIGHTPAREN exp) (lambdaExpression $4 $6))
     ;;;Let
     ((LEFTPAREN LET LEFTPAREN ID VALUE RIGHTPAREN IN exp RIGHTPAREN) (cond
                                                                        [(equal? $5 "λ") (lambdaExpression $4 $8)]
                                                                        [(equal? $5 "&") (andExpression $4 $8)]
                                                                        [(equal? $5 "|") (orExpression $4 $8)]
                                                                        [(equal? $5 "^") (xorExpression $4 $8)]
                                                                        [(equal? $5 "true") (valueExpression $5)]
                                                                        [(equal? $5 "false") (xorExpression $5)]
                                                                        [(equal? $5 "!") (unaryExpression $8)]))
                                                                       

     ;;; IF
     ((LEFTPAREN IF exp THEN exp ELSE exp RIGHTPAREN) (ifExpression $3 $5 $7))
     ;;; Call
     ((LEFTPAREN ID WITH exp RIGHTPAREN) (callExpression $2 $4))
     
     )
    )
   )
  )

(define (evaluate aTree)
  (match aTree
    ;;; Base Case
      [(unaryExpression argumentOne) (not (evaluate argumentOne))]
    ;;; Returns true or false
      [(valueExpression argumentOne) (equal? argumentOne "true")]
    
     [(andExpression argumentOne argumentTwo) (and (evaluate argumentOne) (evaluate argumentTwo))]
     [(orExpression argumentOne argumentTwo) (or (evaluate argumentOne) (evaluate argumentTwo))]
    [(xorExpression argumentOne argumentTwo) (xor (evaluate argumentOne) (evaluate argumentTwo))]
    [(ifExpression firstArgument secondArgument thirdArgument) (if (evaluate firstArgument) (evaluate secondArgument) (evaluate thirdArgument))]
  ;;;  [(lambdaExpression firstArgument secondArgument) (lamda (evaluate argumentOne) (evaluate argumentTwo))]

  
))

;;;(evaluate (expparser (lex-this constantBoolLexer testName)))
(define nottest (open-input-string "(! false)"))
(define andTest (open-input-string "(false & true)"))
(define orTest (open-input-string "(false or true)"))
(define xorTest (open-input-string "(false ^ true)"))
(define ifTest (open-input-string "(if true then true else false)"))

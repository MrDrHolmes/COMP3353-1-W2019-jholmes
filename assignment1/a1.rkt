#lang racket

(provide (all-defined-out))

;; AST -----------------------------------------------------------------------------------------------

(struct ast-node (val) #:transparent)
(struct ast-expr-node (op lc rc) #:transparent)



;; LEXING CODE ---------------------------------------------------------------------------------------

; A token can be one of 5 types:
; 1. A left parenthesis     'lparen
; 2. A right parenthesis    'rparen
; 3. A digit                'digit
; 4. An operator            'op
; 5. An end of input marker 'eof
; Anything else will cause an error upon creation.
(struct token (type repr) #:transparent
   #:guard (λ (type repr struct-name)
     (if (not (is-token-type? type))
         (error "expected a proper token-type which is-token-type? returns true from, got" type)
         (if (and (not (eq? eof repr)) (not (char? repr)))
             (error "expected a char? or eof? for token-repr, got" repr)
             (values type repr))))) ;(values) sets the values of our struct

; Our function to check if our token is valid.
(define (is-token-type? t)
  (or (equal? t 'op)
      (equal? t 'lparen)
      (equal? t 'rparen)
      (equal? t 'digit)
      (equal? t 'eof)))

; This function helps us tokenize with the following 'lexstr' function.
(define (get-next-token input-port)
  (let ([next (read-char input-port)])
    (cond
      [(equal? next #\()                                           (token 'lparen next)]
      [(equal? next #\))                                           (token 'rparen next)]
      [(equal? next #\+)                                               (token 'op next)]
      [(equal? next #\*)                                               (token 'op next)]
      [(equal? next #\1)                                            (token 'digit next)]
      [(equal? next #\2)                                            (token 'digit next)]
      [(equal? next #\3)                                            (token 'digit next)]
      [(equal? next #\4)                                            (token 'digit next)]
      [(equal? next #\5)                                            (token 'digit next)]
      [(equal? next #\6)                                            (token 'digit next)]
      [(equal? next #\7)                                            (token 'digit next)]
      [(equal? next #\8)                                            (token 'digit next)]
      [(equal? next #\9)                                            (token 'digit next)]
      [(equal? next #\0)                                            (token 'digit next)]
      [(equal? next eof)                                              (token 'eof next)]
      [else (raise-syntax-error #f (string-append "Unexpected syntax: " (string next)))])))

; Returns a zero argument function that generates tokens from the provided string when called.
(define (lexstr str)
 (let ([input (open-input-string str)])
 (λ () (get-next-token input))))



;; PARSING CODE --------------------------------------------------------------------------------------
;; "By far, the hardest part of this." ~Jeff

; Starts the parsing process. Builds the AST.
; A program can be an expression, a digit, or empty.
(define (parser lex)
  (let* ([tok            (lex)]
         [typ (token-type tok)]
         [val (token-repr tok)])
    (cond
      [(equal? typ 'lparen)
       (let ([left  (parse-expr lex)]
             [op      (parse-op lex)] ; parse-op returns a node with an operator
             [right (parse-expr lex)]
             [close  (parse-end lex)]); parse-end checks for a right parenthesis
         (ast-expr-node op left right))]
      [(equal? typ 'digit) (ast-node (- (char->integer val) 48))]
      [(equal? typ 'eof)                                     '()]
      [else      (error "Expected token 'lparen, got" typ)])))

; Returns an expression node.
(define (parse-expr lex)
  (let* ([tok            (lex)]
         [typ (token-type tok)]
         [val (token-repr tok)])
    (cond
      [(equal? typ 'lparen)
       (let ([left  (parse-expr lex)]
             [op      (parse-op lex)] 
             [right (parse-expr lex)]
             [close  (parse-end lex)])
         (ast-expr-node op left right))]
      [(equal? typ 'digit) (ast-node (- (char->integer val) 48))]
      [else (error "Expected expression, got" typ)])))

; Returns a node with an operator.
(define (parse-op lex)
  (let* ([tok            (lex)]
         [typ (token-type tok)]
         [val (token-repr tok)])
    (cond
      [(equal? typ 'op)                      val]
      [else (error "Expected operator, got" typ)])))

; Checks for a closing right parenthesis.
(define (parse-end lex)
  (let* ([tok            (lex)]
         [typ (token-type tok)])
    (if (equal? typ 'rparen)
        '()
        (error "Expected closing parenthesis, got" typ))))



;; EVALUATION CODE -----------------------------------------------------------------------------------
;; We did this in class, but I want (have) to figure this out myself.

(define (eval expr)
  (match expr
    [(ast-expr-node op e1 e2)
     (if (equal? op #\+)
         (+ (eval e1) (eval e2))
         (* (eval e1) (eval e2)))]
    [(ast-node val) val]))

(define (evalstr in)
  (eval (parser (lexstr in))))
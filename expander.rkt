#lang br/quicklang
(require racket/syntax)

(provide (rename-out [b-module-begin #%module-begin]))
(define-macro (b-module-begin (expres EXPR))
  #'(#%module-begin EXPR))

(define-macro (id X) #'#'X)

(struct Symbol (name pubname)
  #:methods gen:custom-write
  [(define (write-proc self outport mode)
     (fprintf outport ":~a:~a" (Symbol-pubname self) (Symbol-name self)))])

(define-macro-cases quot
  [(quot)           #'(Symbol #f #f)]
  [(quot (pub))     #'(Symbol #f #f)]
  [(quot (pub P))   #'(Symbol #f #'P)]
  [(quot (pub) Q)   #'(Symbol #'Q #f)]
  [(quot (pub P) Q) #'(Symbol #'Q #'P)]
  [(quot Q)         #'(Symbol #'Q #'Q)])

(define (apply1 f x) (cons f (list x)))
(define braces list)

(define parens identity)
(define int    identity)

(provide quot apply1 braces parens id int)

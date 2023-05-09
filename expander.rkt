#lang br/quicklang
(require racket/syntax)

(provide (rename-out [b-module-begin #%module-begin]))
(define-macro (b-module-begin (expres EXPR))
  #'(#%module-begin EXPR))

(define (debug x)
  (println x) x)

(define vars (make-hash))

(struct Object (value props))

(struct tag (keyword name values) #:transparent)

(define-macro braces #'syntax)

(define-macro-cases quot
  [(quot)           #'(tag ': ': (list))]
  [(quot (pub))     #'(tag ': ': (list))]
  [(quot (pub) Q)   #'(tag ': 'Q (list))]
  [(quot (pub P))   #'(tag 'P ': (list))]
  [(quot (pub P) Q) #'(tag 'P 'Q (list))]
  [(quot Q)         #'(tag 'Q 'Q (list))])

(define string string-append)
(define parens identity)
(define int    identity)
(define dec    identity)

(define-macro (id X)
  #'(let ([var (hash-ref vars (syntax->datum #'X) #f)])
      (or var X)))

(define-macro (err-not-function STX)
  #'(err-cant-apply STX "not a function"))

(define-macro (err-cant-apply STX MSG)
  #'(parameterize ([current-syntax-context #'|can't apply|])
      (wrong-syntax STX MSG)))

(define (synt stx value)
  (datum->syntax #f value (struct-copy srcloc (syntax-srcloc stx))))

(define-macro (blame X)
  #'(pattern-case X
                  [(quot Q)   (synt X (string->symbol (string-append ":" (symbol->string (syntax->datum #'Q)))))]
                  [(string S) (synt X (foldl string-append "" (cdr (syntax->datum #'S))))]
                  [(int N)    #'N]
                  [(dec N)    #'N]))

(define-macro (app F X)
   #'(let ([f F])
       (cond
         [(procedure? f) (f X)]
         [(tag? f) (if (syntax? X) 
                       (let ([varname (tag-name f)])
                         (hash-set! vars varname varname)
                         (eval-syntax #`(lambda (#,varname) #,(eval-syntax X))))
                       (struct-copy tag f [values (append (tag-values f) (list X))]))]
         [else (err-not-function (synt #'F f))])))

(define-macro comma1 #'app)
(define-macro apply1 #'app)
(define-macro apply3 #'app)

(provide comma1 apply1 apply3 parens braces quot id int dec string)

#lang br/quicklang
(require racket/syntax)
(require lens/common lens/data/struct)

(provide (rename-out [b-module-begin #%module-begin]))
(define-macro (b-module-begin EXPR)
  #'(#%module-begin EXPR))

(define (log x)
  (println x) x)

(define vars (make-hash))
(define warnings (list))

(struct/lens ref  (name)                           #:transparent)
(struct/lens tag  (key name keyloc nameloc srcloc) #:transparent)
(struct/lens tage (tags keys names values)         #:transparent)
(struct/lens lamb (signature vars body)            #:transparent)

(define (er e)
  (parameterize ([current-syntax-context #'Error])
                (wrong-syntax (err-this) "~a" e)))

(struct err (this)                 #:transparent)
(struct already-defined err (here) #:transparent)

(define-macro (braces BODY)
  #'(lamb (tage (list) (make-hash) (make-hash) (list))
          (list)
          #'BODY))

(define (warn! w)
  (set! warnings (append warnings (list w))))

(define-macro-cases quark
  [(quark :      ) #`(tag: #f  #f  #'#,caller-stx)]
  [(quark :   :  ) #`(tag: #f  #f  #'#,caller-stx)]
  [(quark : K :  ) #`(tag: #'K #f  #'#,caller-stx)]
  [(quark :   : N) #`(tag: #f  #'N #'#,caller-stx)]
  [(quark : K : N) #`(tag: #'K #'N #'#,caller-stx)]
  [(quark     : N) #`(tag: #'N #'N #'#,caller-stx)])

(define (tag: keyloc nameloc srcloc)
  (tag (syntax->datum keyloc)
       (syntax->datum nameloc)
       keyloc nameloc srcloc))

(define string string-append)
(define int    identity)
(define dec    identity)
(define parens identity)
(define comma  parens)

(define-macro (id X)
  #'(ref 'X))

(define-macro (err-not-function STX)
  #'(err-cant-apply STX "not a function"))

(define-macro (err-cant-apply STX MSG)
  #'(parameterize ([current-syntax-context #'|can't apply|])
      (wrong-syntax STX MSG)))

(define (synt stx value)
  (datum->syntax #f value (struct-copy srcloc (syntax-srcloc stx))))

(define-macro (blame X)
  #'(pattern-case X
                  [(quark Q)   (synt X (string->symbol (string-append ":" (symbol->string (syntax->datum #'Q)))))]
                  [(string S) (synt X (foldl string-append "" (cdr (syntax->datum #'S))))]
                  [(int N)    #'N]
                  [(dec N)    #'N]))

(define (lookup var)
  (hash-ref vars var #f))

(define (def tag value)
  (let* ([name (syntax->datum (tag-name tag))]
         [existing (lookup name)])
    (if existing
        (wrong-syntax existing "Already defined")
        (hash-set! vars name value)) (println  vars)))

(define (run x lamb)
  (lens-set/list lamb
                 lamb-tage-lens (lamb-tage lamb)
                 lamb-vars-lens (lamb-vars lamb))
  (println lamb)
  (println (tag-name (car (tage-tags (lamb-tage lamb))))))

(define (dict k v)
  (make-hash `((,k . ,v))))

(define-macro (map/add KEY VALUE -dupe? HANDLER)
  #'(let ([existing (hash-ref )])))

(define-macro-cases tage:
  [(tage: TAG)
   #'(tage: TAG (list))]

  [(tage: TAG VALUES)
   #'(tage (list TAG)
           (dict (tag-key TAG) (tag-keyloc TAG))
           (dict (tag-name TAG) (tag-nameloc TAG))
           VALUES)]

  [(tage: TAGE -add TAG PROC -dupe? HANDLE-DUPE)
   #'(let* ([key (tag-key TAG)]
            [name (tag-name TAG)]
            [keys (tage-keys TAGE)]
            [names (tage-names TAGE)]
            [existing-key (hash-ref keys key #f)]
            [existing-name (hash-ref names name #f)])
       (cond
         [existing-key (HANDLE-DUPE (tag-keyloc TAG) existing-key))]
         [existing-name (HANDLE-DUPE (tag-nameloc TAG) existing-name)]
         [else
          (hash-set! keys key TAG)
          (hash-set! names name TAG)
          (tage (PROC) keys names (tage-values TAGE))]))]

  [(tage: TAGE -append TAG)
   #'(tage: TAGE -add TAG (lambda () (append (tage-tags TAGE) (list TAG)))
            -dupe? (lambda (new existing) (er (already-defined new #:here existing))))]

  [(tage: TAGE -prepend TAG)
   #'(tage: TAGE -add TAG (lambda () (cons TAG (tage-tags TAGE))
            -dupe? (lambda (new existing) (er (already-defined existing #:here new))]

  [(tage: TAGE1 -extends TAGE2)
   #'(let* ([key (tag-key TAG)]
            [name (tag-name TAG)]
            [keys (tage-keys TAGE)]
            [names (tage-names TAGE)]
            [existing-key (hash-ref keys key #f)]
            [existing-name (hash-ref names name #f)])
       (cond
         [existing-key (err (already-defined existing-key (tag-keyloc TAG)) existing-key)]
         [existing-name (err (already-defined existing-name (tag-nameloc TAG)) existing-name)]
         [else
          (hash-set! keys key TAG)
          (hash-set! names name TAG)
          (tage (cons TAG (tage-tags TAGE)) keys names (tage-values TAGE))]))])

(define-macro (lamb: LAMB .tags+ TAG)
  #'(lens-set lamb-tage-lens LAMB (tage: (lamb-tage lamb) -prepend tag)))

(define-macro (app F X)
  #`(let ([f F]
          [x X])
      (cond
        [(tag? f) (cond
                    [(tag? x) (tage: (tage: x) -prepend f)]
                    [(tage? x) (tage: x -prepend f)]
                    [(lamb? x) (lamb: x .tage/prepend f)]
                    [else (tage: f (list x))])]
        [(tage? f) (cond
                     [(tag? x) (tage: f -append x)]
                     [(tage? x) (tage/merge f x)]
                     [(lamb? x) (lamb: x .tage/prepend* f)]
                     [else (tage: f .values/append x)])]
        [(lamb? f) (run x f)]
        [else (err-not-function (synt #'F f))])))

(define-macro comma1 #'app)
(define-macro apply1 #'app)
(define-macro apply3 #'app)

(define (expres e)
  (cond [(not (empty? warnings))
         (for-each (lambda (warning)
                     (display "Warning: ")
                     (displayln warning))
                   warnings)])
  e)

(provide expres comma comma1 apply1 apply3 parens braces quark id int dec string)

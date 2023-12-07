#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (alpha          (:/ #\a #\z #\A #\Z))
  (alnums?    (:* (:/ #\a #\z #\A #\Z #\0 #\9)))
  (alnums     (:+ (:/ #\a #\z #\A #\Z #\0 #\9)))
  (digits     (:+ (:/                 #\0 #\9)))
  (opchar         (char-set "+/-><=*\\~?!&|^#%$@_"))
  (line-break      (char-set "\r\n"))
  (spacetabs  (:+ (:or #\space #\tab)))
  (spacetabs? (:? spacetabs))
  (newline    (:seq spacetabs? (:or "\r\n" "\n")))
  (integer    (:seq digits (:* (:seq #\_ digits))))
  (decimal    (:seq integer #\. integer))
  (identifier (:seq     alpha alnums? (:* (:seq dashes alnums)) prime?))
  (unit       (:seq (:+ alpha)                                  prime?))
  (operator   (:seq (:+ opchar)                                 prime?))
  (indent     (:seq (:+ newline) (:* #\tab)))
  (bublet     (:seq "(:" (:? (:or identifier operator #\:)) #\)))
  (dashes     (:+ #\-))
  (prime?     (:* #\'))
  (d-quote        #\")
  (b-quote        #\`))

(define unit-lexer
  (lexer-srcloc
   [unit     (begin (pop-mode!) (token 'ID (string->symbol lexeme)))]
   [any-char (begin (pop-mode!) (rewind! empty))]
   [(eof)    (begin (pop-mode!) empty)]))

(define main-lexer
  (lexer-srcloc
   [indent (let ([next-level (add1 _level)]
                 [dent (measure-dent!)])
             (cond
               [(> dent next-level) (error-on-indentation next-level)]
               [(= dent next-level) (indent!)]
               [(= dent _level) (token-NEWLINE)]
               [(< dent _level) (reset-level! dent)]))]
   [spacetabs  (token 'SPACE  #f)]
   [#\$        (token 'DOLLAR '$)]
   [#\/        (token 'SLASH  '/)]
   [dashes     (token 'DASH                               (string->symbol lexeme))]
   [operator   (token 'OP                                 (string->symbol lexeme))]
   [identifier (token 'ID                                 (string->symbol lexeme))]
   [bublet     (token 'BUBLET                             (string->symbol lexeme))]
   [integer    (token 'INT (begin (push-mode! unit-lexer) (string->number lexeme)))]
   [decimal    (token 'DEC (begin (push-mode! unit-lexer) (string->number lexeme)))]
   [(:seq d-quote indent) d-block]
   [(:seq b-quote indent) b-block]
   [      d-quote         d-str]
   [      b-quote         b-str]
   ["{," (list (token-LBRACE!) (token 'IT '|,|))]
   [#\(        (token-LPAREN!)]
   [#\)        (token-RPAREN!)]
   [#\{        (token-LBRACE!)]
   [#\}        (token-RBRACE!)]
   [#\[        (token-LSQUARE!)]
   [#\]        (token-RSQUARE!)]
   [#\.        (token 'DOT       '|.|)]
   [#\:        (token 'COLON     '|:|)]
   [#\;        (token 'SEMICOLON '|;|)]
   [#\,        (token 'COMMA     '|,|)]
   [(eof) (if (> _level 0)
              (reset-level! 0) 
              (void))]))

(define-macro (strlex (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(lexer-srcloc CUSTOM-RULES ...
                  [(:+ (:~ line-break CUSTOM-CHARS ...)) (token 'STRING lexeme)]
                  [any-char (token 'STRING lexeme)]))

(define-macro (strlex-i (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(strlex (#\` CUSTOM-CHARS ...)
             [(:seq "`{" spacetabs? "}") (token 'STRING "")]
             [(:seq "`{" spacetabs?) (list (token 'INTERPOLATE 0) (token-LBRACE!))]
             [(:seq "`{" indent "}") (escape-newline)]
             [(:seq "`{" indent) (str-interp (token-LBRACE!) #:dent-or (error-on-indentation))]
             CUSTOM-RULES ...))

(define-macro (str-interp TOKENS ... #:dent-or NODENT)
  #'(let ([current-dent _dent]
          [next-dent (add1 _dent)]
          [new-dent (measure-dent!)])
      (cond
        [(= new-dent next-dent) (let ([token-INTERPOLATE (token 'INTERPOLATE (- current-dent _level))])
                                  (begin (push-mode! (jumpto _level))
                                         (set! _level current-dent)
                                         (list token-INTERPOLATE TOKENS ... (indent!))))]
        [(< new-dent next-dent) NODENT]
        [(> new-dent next-dent) (error-on-indentation next-dent)])))

(define-macro d-str
  #'(token-QUOTE! (strlex-i (d-quote)
                            [d-quote (token-UNQUOTE! lexeme)]
                            [line-break (error-unterminated-string)]
                            [(eof) (error-unterminated-string)])))

(define-macro b-str
  #'(list (token 'BACKTICK '|`|)
          (token-QUOTE! (strlex-i (#\( #\) #\, line-break)
                                  [(:or #\) #\, line-break) (rewind! (token-UNQUOTE!))]
                                  [#\( (begin (push-mode! find-endparen) (token 'STRING lexeme))]
                                  [(eof) (cons (token-UNQUOTE!)
                                               (reset-level! 0))]))))

(define find-endparen (strlex-i (#\( #\) line-break)
                                [#\( (begin (push-mode! find-endparen) (token 'STRING lexeme))]
                                [#\) (begin (pop-mode!)                (token 'STRING lexeme))]
                                [line-break (error "Missing closing parenthesis")]
                                [(eof)      (error "Missing closing parenthesis")]))

(define-macro (blockstr CUSTOM-RULES ...)
  #'(strlex ()
            CUSTOM-RULES ...
            [indent (extract-indent!)]))

(define-macro (blockstr-i CUSTOM-RULES ...)
  #'(strlex-i ()
              [(:seq #\` indent) (str-interp #:dent-or (rewind! #:until "`"))]
              CUSTOM-RULES ...
              [indent (extract-indent!)]))

(define-macro d-block
  #'(append (list (token-QUOTE! (lexer-srcloc [d-quote  (token-UNQUOTE!)]
                                              [any-char (error-unterminated-string)]
                                              [(eof)    (error-unterminated-string)]))
                  (indent!      (blockstr     [(eof)    (error-unterminated-string)])))
            (-1LF (extract-indent!))))

(define-macro b-block
  #'(append (list (token 'BACKTICK '|`|)
                  (token-QUOTE! dedent->unquote)
                  (indent! (blockstr-i [(eof) (reset-level! 0)])))
            (-1LF (extract-indent!))))

(define-macro (append-if COND ITEM)
  #'(if COND (list ITEM) empty))

(define-macro (concat-if COND LIST)
  #'(if COND LIST empty))

(define-macro (starts-with? PREFIX STR)
  #'(and (not (equal? eof STR))
         (string-prefix? STR PREFIX)))

(define-macro-cases rewind!
  [(rewind! TOKEN)        #'(rewind! 0 TOKEN)]
  [(rewind! #:until STR)  #'(rewind! (string-length STR) (token 'STRING STR))]
  [(rewind! EATLEN TOKEN) #'(let ([newpos (+ (position-offset start-pos) EATLEN)])
                              (file-position input-port (sub1 newpos))
                              (set-port-next-location! input-port (position-line start-pos) 
                                                       (+ (position-col start-pos) EATLEN) newpos)                                 
                              TOKEN)])

(define-macro-cases error
  [(error MSG LINE COL OFFSET LENGTH) #'(raise-read-error MSG (file-path) LINE COL OFFSET LENGTH)]
  [(error MSG) #'(error MSG
                        (position-line start-pos)
                        (position-col start-pos)
                        (position-offset start-pos)
                        (if (string? lexeme) (string-length lexeme) 0))])

(define-macro-cases error-on-indentation
  [(error-on-indentation)     #'(error-on-indentation 1 _dent "Insufficient indentation")]
  [(error-on-indentation COL) #'(error-on-indentation COL (- _dent COL) "Too much indentation")]
  [(error-on-indentation COL LENGTH MSG) #'(error MSG (position-line end-pos) COL 
                                                  (- (position-offset end-pos) LENGTH) LENGTH)])

(define-macro error-on-syntax
  #'(error (string-append "Syntax erroror: " lexeme)))

(define-macro error-unterminated-string
  #'(error "Unterminated string (missing closing quote)"))

(define (debug x)
  (println x) x)

(define _last-token null)
(define _pending-tokens (list))
(define _indents (list))
(define _modestack (list))
(define _mode main-lexer)

(define (push-mode! lexer)
  (push! _modestack _mode)
  (set! _mode lexer))

(define (pop-mode!)
  (set! _mode (pop! _modestack))
  (if (jumpto? _mode)
      (begin (set! _level (jumpto-level _mode))
             (pop-mode!))
      void)
  _mode)

(struct jumpto (level))
(define _level 0)
(define _dent 0)

(define (indent! [lexer main-lexer])
  (set! _level (add1 _level))
  (push-mode! lexer)
  (token 'INDENT _mode))

(define (dedent!)
  (set! _level (sub1 _level))
  (pop-mode!)
  (append (list (token 'DEDENT _mode))
          (append-if (equal? _mode dedent->unquote) (dedent->unquote))))

(define dedent->unquote
  (lambda () (token-UNQUOTE!)))

(define-macro (measure-dent!)
  #'(begin (set! _dent (string-length (last (string-split lexeme "\n" #:trim? #f))))
           _dent))

(define-macro (reset-level! CAP)
  #'(let* ([dedents '()]
           [_ (while (> _level CAP)
                     (set! dedents (append dedents (dedent!))))]
           [num-feed (if (empty? dedents) numLF (sub1 numLF))])
      (append (concat-if (> num-feed 0)
                         (make-list num-feed (token-NEWLINE)))
              dedents
              (append-if (equal? _mode main-lexer) (token-NEWLINE))
              (append-if (and (> _level 0)
                              (> _dent _level))
                         (token-STRING (- _dent _level) #\tab)))))

(define (open-group! TOKEN)
  (push! _indents _level)
  TOKEN)

(define-macro (close-group! TOKEN)
  #'(let ([expected-level (if (empty? _indents)
                              (error "No matching pair")
                              (pop! _indents))])
      (if (> _level expected-level)
          (append (reset-level! expected-level)
                  (list TOKEN))
          TOKEN)))

(define-macro (token-LPAREN!)
  #'(open-group! (token 'LPAREN '|(|)))

(define-macro (token-RPAREN!)
  #'(close-group! (token 'RPAREN '|)|)))

(define-macro (token-LSQUARE!)
  #'(open-group! (token 'LSQUARE '|[|)))

(define-macro (token-RSQUARE!)
  #'(close-group! (token 'RSQUARE '|]|)))

(define (token-LBRACE! [lexer main-lexer])
  (push-mode! lexer)
  (open-group! (token 'LBRACE '|{|)))

(define-macro token-RBRACE!
  #'(cond [(empty? _modestack) (error "No matching pair")]
          [else (pop-mode!) (close-group! (token 'RBRACE '|}|))]))

(define-macro (token-QUOTE! LEXER)
  #'(begin (push-mode! LEXER)
           (token 'QUOTE lexeme)))

(define-macro-cases token-UNQUOTE!
  [(token-UNQUOTE!)        #'(token-UNQUOTE! '||)]
  [(token-UNQUOTE! LEXEME) #'(begin (pop-mode!)
                                    (token 'UNQUOTE LEXEME))])

(define (token-STRING count char)
  (token 'STRING (make-string count char)))

(define (token-NEWLINE)
  (token 'NEWLINE "\n"))

(define-macro line-diff
  #'(- (position-line end-pos) (position-line start-pos)))

(define-macro numLF
  #'(- (position-line end-pos)
       (position-line start-pos)))

(define-macro -1LF
  #'(let ([indent extract-indent!])
      (if (equal? 'NEWLINE (token-struct-type (car indent)))
          (cdr indent) indent)))

(define-macro extract-indent!
  #'(reset-level! (min _level (measure-dent!))))

(define-macro escape-newline
  #'(let ([current-dent _dent]
          [new-dent (sub1 (measure-dent!))])
      (cond
        [(< new-dent current-dent) (error-on-indentation)]
        [(= new-dent current-dent) (token 'STRING "")]
        [(> new-dent current-dent) (error-on-indentation current-dent)])))

(define (bilang-lexer ip)
  (set! _last-token
        (if (empty? _pending-tokens)
            (let* ([produce (_mode ip)]
                   [tokens (and (srcloc-token? produce) 
                                (srcloc-token-token produce))])
              (cond
                [(empty? tokens) (bilang-lexer ip)]
                [(list? tokens)
                 (let ([prods (map (lambda (t)
                                     (if (srcloc-token? t) t
                                         (srcloc-token t (srcloc-token-srcloc produce))))
                                   tokens)])
                   (set! _pending-tokens (cdr prods))
                   (car prods))]
                [else produce]))
            (pop! _pending-tokens)))
  _last-token)

(provide bilang-lexer)

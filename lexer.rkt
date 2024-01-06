#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (alpha          (:/ #\a #\z #\A #\Z))
  (alnums?    (:* (:/ #\a #\z #\A #\Z #\0 #\9)))
  (alnums     (:+ (:/ #\a #\z #\A #\Z #\0 #\9)))
  (digits     (:+ (:/                 #\0 #\9)))
  (symbol     (:+ (char-set "+/-><=*\\~?!&|^#%$@_")))
  (line-break     (char-set "\r\n"))
  (spacetabs  (:+ (:or #\space #\tab)))
  (spacetabs? (:? spacetabs))
  (newline    (:seq spacetabs? (:or "\r\n" "\n")))
  (integer    (:seq digits (:* (:seq #\_ digits))))
  (decimal    (:seq integer #\. integer))
  (identifier (:seq     alpha alnums? (:* (:seq dashes alnums))))
  (unit       (:seq (:+ alpha) (:? prime)))
  (nextloc    (:seq (:+ newline) (:* #\tab)))
  (dashes     (:+ #\-))
  (prime      (:+ #\'))
  (s-quote        #\')
  (d-quote        #\")
  (grave          #\`)
  (comma          #\,))

(define prime-lexer
  (sublexer
   [prime (begin (pop-mode!) (token 'PRIME (string->symbol lexeme)))]))

(define unit-lexer
  (sublexer
   [unit (begin (pop-mode!) (token 'IDENTIFIER (string->symbol lexeme)))]))

(define main-lexer
  (lexer-srcloc
   [nextloc (let ([next-level (add1 _level)]
                  [current-dent (measure-dent!)])
              (cond
                [(> current-dent next-level) (error-on-indentation next-level)]
                [(= current-dent next-level) (indent!)]
                [(= current-dent _level) (token-NEWLINE)]
                [(< current-dent _level) (dedent-to! current-dent)]))]
   [spacetabs  (token 'SPACE #f)]
   [#\?        (token 'QUESTION-MARK (begin (push-mode! prime-lexer) '?))]
   [#\$        (token 'DOLLAR        (begin (push-mode! prime-lexer) '$))]
   [#\/        (token 'SLASH         (begin (push-mode! prime-lexer) '/))]
   [dashes     (token 'DASH          (begin (push-mode! prime-lexer) (string->symbol lexeme)))]
   [symbol     (token 'SYMBOL        (begin (push-mode! prime-lexer) (string->symbol lexeme)))]
   [identifier (token 'IDENTIFIER    (begin (push-mode! prime-lexer) (string->symbol lexeme)))]
   [integer    (token 'INTEGER       (begin (push-mode! unit-lexer)  (string->number lexeme)))]
   [decimal    (token 'DECIMAL       (begin (push-mode! unit-lexer)  (string->number lexeme)))]
   [(:seq s-quote nextloc) s-block]
   [(:seq d-quote nextloc) d-block]
   [(:seq   grave nextloc) g-block]
   [      s-quote          s-str]
   [      d-quote          d-str]
   [        grave          g-str]
   ["(:" (list (token-LPAREN!) (token 'PROTON '|:|))]
   ["{," (list (token-LBRACE!) (token 'IT     '|,|))]
   [#\(        (token-LPAREN!)]
   [#\)        (token-RPAREN!)]
   [#\{        (token-LBRACE!)]
   [#\}        (token-RBRACE!)]
   [#\[        (token-LBRACKET!)]
   [#\]        (token-RBRACKET!)]
   [#\.        (token 'DOT       '|.|)]
   [#\:        (token 'COLON     '|:|)]
   [#\;        (token 'SEMICOLON '|;|)]
   [#\,        (token 'COMMA     '|,|)]
   [(eof) (if (> _level 0)
              (dedent-to! 0) 
              (void))]))

(define-macro (sublexer RULES ...)
  #'(lexer-srcloc RULES ...
                  [any-char (begin (pop-mode!) (rewind! empty))]
                  [(eof)    (begin (pop-mode!) empty)]))

(define-macro (strlex (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(lexer-srcloc CUSTOM-RULES ...
                  [(:+ (:~ line-break CUSTOM-CHARS ...)) (token 'STRING lexeme)]
                  [line-break (error-unterminated-string)]
                  [any-char (token 'STRING lexeme)]))

(define-macro (strlex-i (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(strlex (#\` CUSTOM-CHARS ...)
            [(:seq "`{" spacetabs? "}") (token 'STRING "")]
            [(:seq "`{" spacetabs?)     (list (token 'INTERPOLATE 0) (token-LBRACE!))]
            [(:seq "`{" nextloc "}")    (escape-newlines)]
            [(:seq "`{" nextloc)        (str-interp (token-LBRACE!) #:dent-or (error-on-indentation))]
            CUSTOM-RULES ...))

(define-macro (g-lex (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(list (token 'GRAVE '|`|)
          (token-QUOTE! (strlex-i (CUSTOM-CHARS ...)
                                  CUSTOM-RULES ...
                                  [line-break (token-UNQUOTE! lexeme)]
                                  [(eof) (cons (token-UNQUOTE!)
                                               (dedent-to! 0))]))))

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

(define find-endparen (strlex-i (#\( #\) line-break)
                                [#\( (begin (push-mode! find-endparen) (token 'STRING lexeme))]
                                [#\) (begin (pop-mode!)                (token 'STRING lexeme))]
                                [line-break error-missing-endparen]
                                [(eof)      error-missing-endparen]))

(define-macro s-str
  #'(token-QUOTE! (strlex (s-quote)
                          [s-quote (token-UNQUOTE! lexeme)]
                          [(eof) (error-unterminated-string)])))

(define-macro d-str
  #'(token-QUOTE! (strlex-i (d-quote)
                            [d-quote (token-UNQUOTE! lexeme)]
                            [(eof) (error-unterminated-string)])))

(define-macro g-str
  #'(let ([prev-token (if _last-token
                          (token-struct-type (srcloc-token-token _last-token))
                          #f)])
      (if (member prev-token '(IDENTIFIER SYMBOL DOLLAR DASH SLASH QUESTION-MARK))
          (let ([next-char (read-char input-port)])
            (cond
              ((equal? next-char #\() g-group)
              ((equal? next-char #\space) g-span)
              (else (begin (rewind! 1 empty) g-word))))
          g-line)))

(define-macro g-word
  #'(g-lex (comma) [comma (rewind! (token-UNQUOTE!))]))

(define-macro g-span
  #'(g-lex (#\space) [" ," (rewind! (token-UNQUOTE!))]))

(define-macro g-group
  #'(g-lex (#\( #\))
           [#\(        (begin (push-mode! find-endparen) (token 'STRING lexeme))]
           [#\)        (token-UNQUOTE!)]
           [line-break error-missing-endparen]
           [(eof)      error-missing-endparen]))

(define-macro g-line
  #'(g-lex ()))

(define-macro (blockstr CUSTOM-RULES ...)
  #'(strlex ()
            CUSTOM-RULES ...
            [nextloc (extract-white!)]))

(define-macro (blockstr-i CUSTOM-RULES ...)
  #'(strlex-i ()
              [(:seq #\` nextloc) (str-interp #:dent-or (rewind! #:until "`"))]
              CUSTOM-RULES ...
              [nextloc (extract-white!)]))

(define-macro s-block
  #'(append (list (token-QUOTE! (lexer-srcloc [s-quote  (token-UNQUOTE!)]
                                              [any-char (error-unterminated-string)]
                                              [(eof)    (error-unterminated-string)]))
                  (indent!      (blockstr     [(eof)    (error-unterminated-string)])))
            (minus-one-newline (extract-white!))))

(define-macro d-block
  #'(append (list (token-QUOTE! (lexer-srcloc [d-quote  (token-UNQUOTE!)]
                                              [any-char (error-unterminated-string)]
                                              [(eof)    (error-unterminated-string)]))
                  (indent!      (blockstr-i   [(eof)    (error-unterminated-string)])))
            (minus-one-newline (extract-white!))))

(define-macro g-block
  #'(append (list (token 'GRAVE '|`|)
                  (token-QUOTE! 'unquote-on-dedent)
                  (indent! (blockstr-i [(eof) (dedent-to! 0)])))
            (minus-one-newline (extract-white!))))

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

(define-macro error-missing-endparen
  #'(error "Missing closing parenthesis"))

(define (debug x)
  (println x) x)

(define _last-token #f)
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
          (append-if (equal? _mode 'unquote-on-dedent) (token-UNQUOTE!))))

(define (count-leading-tab string [start 0])
  (if (and (< start (string-length string))
           (equal? #\tab (string-ref string start)))
      (add1 (count-leading-tab string (add1 start)))
      0))

(define-macro (measure-dent!)
  #'(let ([lastline (last (string-split lexeme "\n" #:trim? #f))])
      (set! _dent (count-leading-tab lastline))
      _dent))

(define-macro (dedent-to! LEVEL)
  #'(let* ([linediff (- (position-line end-pos)
                        (position-line start-pos))]
           [num-dedents-needed (- _level LEVEL)]
           [num-newlines-needed (if (and (> num-dedents-needed 0)
                                         (> linediff 0))
                                    (sub1 linediff) ; last one is dedent(s)
                                    linediff)])
      (append (make-list num-newlines-needed (token-NEWLINE))
              (let ([dedent-tokens '()])
                (while (> _level LEVEL)
                       (set! dedent-tokens (append dedent-tokens (dedent!))))
                dedent-tokens)
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
          (append (dedent-to! expected-level)
                  (list TOKEN))
          TOKEN)))

(define-macro (token-LPAREN!)
  #'(open-group! (token 'LPAREN '|(|)))

(define-macro (token-RPAREN!)
  #'(close-group! (token 'RPAREN '|)|)))

(define-macro (token-LBRACKET!)
  #'(open-group! (token 'LBRACKET '|[|)))

(define-macro (token-RBRACKET!)
  #'(close-group! (token 'RBRACKET '|]|)))

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

(define (minus-one-newline tokens)
  (if (equal? 'NEWLINE (token-struct-type (car tokens)))
      (cdr tokens)
      tokens))

(define-macro line-diff
  #'(- (position-line end-pos) (position-line start-pos)))

(define-macro extract-white!
  #'(dedent-to! (min _level (measure-dent!))))

(define-macro escape-newlines
  #'(let ([current-dent _dent]
          [new-dent (measure-dent!)])
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

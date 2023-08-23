#lang br
(require brag/support syntax/readerr)

(define-lex-abbrevs
  (digits (:+ (:/ #\0 #\9)))
  (integer (:seq digits (:* (:seq #\_ digits))))
  (decimal (:seq integer #\. integer))
  (alpha       (:/ #\a #\z #\A #\Z))
  (alnums? (:* (:/ #\a #\z #\A #\Z #\0 #\9)))
  (alnums  (:+ (:/ #\a #\z #\A #\Z #\0 #\9)))
  (op      (:+ (char-set "+/-><=*\\~?!&|^#%$@_")))
  (identifier (:seq alpha alnums? (:* (:seq #\- alnums)) prime?))
  (shortid    (:seq alpha alnums?                        prime?))
  (operator   (:seq                         op           prime?))
  (newline-char (char-set "\r\n"))
  (newline (:seq (:? spacetabs) (:or "\r\n" "\n")))
  (nextloc (:seq (:+ newline) (:* #\tab)))
  (slash   #\/)
  (s-quote #\')
  (d-quote #\")
  (b-quote #\`)
  (prime? (:* s-quote))
  (space/tab (:or #\space #\tab))
  (spacetabs (:+ space/tab))
  (spacetabs? (:* space/tab)))

(define main-lexer
  (lexer-srcloc
   [nextloc (let ([next-level (add1 _level)]
                  [dent (measure-dent!)])
              (cond
                [(> dent next-level) (error-on-indentation next-level)]
                [(= dent next-level) (indent!)]
                [(= dent _level) (token-NEWLINE)]
                [(< dent _level) (reset-level! dent)]))]
   [spacetabs  (token 'SPACE lexeme)]
   [slash      (token 'SLASH (string->symbol lexeme))]
   [operator   (token 'OP (string->symbol lexeme))]
   [identifier (token 'ID (string->symbol lexeme))]
   [(:seq (:? #\-) integer) (begin (push-mode! shortid-lexer) (token 'INT (string->number lexeme)))]
   [(:seq (:? #\-) decimal) (begin (push-mode! shortid-lexer) (token 'DEC (string->number lexeme)))]
   [(:seq "{" spacetabs? ",") (list (token-LBRACE!) (token 'HOLE '|,|))]
   [(:seq d-quote nextloc) d-block]
   [(:seq b-quote nextloc) b-block]
   [d-quote d-str]
   [b-quote b-str]
   [#\( (token-LPAREN!)]
   [#\) (token-RPAREN!)]
   [#\{ (token-LBRACE!)]
   [#\} (token-RBRACE!)]
   [#\[ (token-LSQUARE!)]
   [#\] (token-RSQUARE!)]
   [#\. (token 'DOT '|.|)]
   [#\: (token 'COLON ':)]
   [#\; (token 'SEMICOLON ''SEMICOLON)]
   [#\, (if (equal? 'INDENT (token-struct-type (srcloc-token-token _last-token)))
            (token 'HOLE '|,|)
            (token 'COMMA '|,|))]
   [(eof) (if (> _level 0)
              (reset-level! 0) 
              (void))]))

(define shortid-lexer
  (lexer-srcloc
   [shortid  (begin (pop-mode!) (token 'ID (string->symbol lexeme)))]
   [any-char (begin (pop-mode!) (rewind! empty))]
   [(eof)    (begin (pop-mode!) empty)]))

(define-macro (str-lexer (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(lexer-srcloc CUSTOM-RULES ...
                  [(:+ (:~ newline-char CUSTOM-CHARS ...)) (token 'STRING lexeme)]
                  [any-char (token 'STRING lexeme)]))

(define-macro (str-lexi (CUSTOM-CHARS ...) CUSTOM-RULES ...)
  #'(str-lexer (#\` CUSTOM-CHARS ...)
               [(:seq "`{" spacetabs? "}") (token 'STRING "")]
               [(:seq "`{" spacetabs?) (list (token 'INTERPOLATE 0) (token-LBRACE!))]
               [(:seq "`{" nextloc "}") (escape-newline)]
               [(:seq "`{" nextloc) (str-interp (token-LBRACE!) #:dent-or (error-on-indentation))]
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
  #'(token-QUOTE! (str-lexi (d-quote)
                            [d-quote (token-UNQUOTE! lexeme)]
                            [newline-char (error-unterminated-string)]
                            [(eof) (error-unterminated-string)])))

(define-macro b-str
  #'(token-QUOTE! (str-lexi (#\,)
                            [newline-char (rewind! (token-UNQUOTE!))]
                            [#\, (rewind! (token-UNQUOTE!))]
                            [(eof) (cons (token-UNQUOTE!)
                                         (reset-level! 0))])))

(define-macro (blockstr STR-LEXER CUSTOM-RULES ...)
  #'(STR-LEXER ()
               CUSTOM-RULES ...
               [nextloc (extract-whites!)]))

(define-macro (blockstr-lexi CUSTOM-RULES ...)
  #'(blockstr str-lexi
              [(:seq #\` nextloc) (str-interp #:dent-or (rewind! #:until "`"))]
              CUSTOM-RULES ...))

(define-macro d-block
  #'(append (list (token-QUOTE! (lexer-srcloc [d-quote (token-UNQUOTE!)]
                                              [any-char (error-unterminated-string)]
                                              [(eof) (error-unterminated-string)]))
                  (indent! (blockstr-lexi [(eof) (error-unterminated-string)])))
            (-1LF (extract-whites!))))

(define-macro b-block
  #'(append (list (token-QUOTE! dedent->unquote)
                  (indent! (blockstr-lexi [(eof) (reset-level! 0)])))
            (-1LF (extract-whites!))))

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
  #'(open-group! (token 'LPAREN "(")))

(define-macro (token-RPAREN!)
  #'(close-group! (token 'RPAREN ")")))

(define-macro (token-LSQUARE!)
  #'(open-group! (token 'LSQUARE "[")))

(define-macro (token-RSQUARE!)
  #'(close-group! (token 'RSQUARE "]")))

(define (token-LBRACE! [lexer main-lexer])
  (push-mode! lexer)
  (open-group! (token 'LBRACE "{")))

(define-macro token-RBRACE!
  #'(cond [(empty? _modestack) (error "No matching pair")]
          [else (pop-mode!) (close-group! (token 'RBRACE "}"))]))

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
  #'(let ([whites extract-whites!])
      (if (equal? 'NEWLINE (token-struct-type (car whites)))
          (cdr whites) whites)))

(define-macro extract-whites!
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

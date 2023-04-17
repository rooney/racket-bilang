#lang br/quicklang
(require "parser.rkt" "tokenizer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
   (println (syntax->datum parse-tree))
  (strip-bindings
   #`(module bilang-mod bilang/expander
       #,parse-tree)))
(module+ reader (provide read-syntax))

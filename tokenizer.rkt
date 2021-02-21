#lang racket
(require br-parser-tools/lex)
(require (prefix-in : br-parser-tools/lex-sre))
(require brag/support)
(define-lex-abbrevs
  (alphanum (:/ "0" "9" "a" "f" "A" "F"))
  (digit (:/ "0" "9"))
  (non-ascii (:/ #\u0 #\u9f))
  (new-line (:or "\n" "\r\n" "\r" "\f"))
  (whitespace (:* (char-set " \t\r\n\f"))) ; Shadows the pre-existing whitespace macro
  (unicode (:: #\\
               (:** 1 6 alphanum)
               (:? (:or "\r\n"
                        (char-set " \n\r\t\f")))))
  (escape (:or unicode
               (:: #\\ (:~ (:or
                            "\n" "\r" "\f" alphanum)))))
  (name-start (:or "_"
                   (:/ "a" "z" "A" "Z")
                   non-ascii escape))
  (name-char (:or "_" "-" alphanum non-ascii escape))
  (name (:+ name-char))
  (ident (:: (:? "-")
             name-start
             (:* name-char)))
  (num (:or (:+ digit)
            (:: (:* digit)
                "."
                (:+ digit))))
  ; Many of the following rules could be made DRYer, but imo that would obscure them
  (double-quote-string (:: #\"
                           (:* (:or (:~ (char-set "\n\r\f\\\""))
                                    (:: #\\ new-line)
                                    escape))
                           #\"))
  (single-quote-string (:: #\'
                           (:* (:or (:~ (char-set "\n\r\f\\\'"))
                                    (:: #\\ new-line)
                                    escape))
                           #\'))
  (string (:or double-quote-string single-quote-string))
  (bad-double-quote-string (:: #\"
                               (:* (:or (:~ (char-set "\n\r\f\\\""))
                                        (:: #\\ new-line)
                                        escape))
                               (:? #\\)))
  (bad-single-quote-string (:: #\'
                               (:* (:or (:~ (char-set "\n\r\f\\\'"))
                                        (:: #\\ new-line)
                                        escape))
                               (:? #\\)))
  (bad-string (:or bad-double-quote-string bad-single-quote-string))
  (bad-comment-0 (:: "/*"
                     (:* (:~ "*"))
                     (:+ "*")
                     (:* (:: (:~ (char-set "/*"))
                             (:* (:~ "*"))
                             (:+ "*")))))
  (bad-comment-1 (:: "/*"
                     (:* (:~ "*"))
                     (:* (:: (:+ "*")
                             (:~ (char-set "/*"))
                             (:* (:~ "*"))))))
  (bad-comment (:or bad-comment-0 bad-comment-1))
  (uri-start (:: (:or "u" "U")
                 (:or "r" "R")
                 (:or "l" "L")
                 "(" whitespace))
  (bad-uri-0 (:: uri-start
                 (:* (:or (char-set "!#$%&*-~")
                          non-ascii escape))
                 whitespace))
  (bad-uri-1 (:: uri-start string whitespace))
  (bad-uri-2 (:: uri-start bad-string))
  (bad-uri (:or bad-uri-0 bad-uri-1 bad-uri-2)))
(define css-lexer
  (lexer
   [whitespace 1]
   [any-char 2]))
(define (make-tokenizer port)
  (thunk (css-lexer port)))
(define (test x) (apply-tokenizer-maker make-tokenizer x))
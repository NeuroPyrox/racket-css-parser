#lang racket
(require br-parser-tools/lex)
(require (prefix-in : br-parser-tools/lex-sre))
(require brag/support)
; Pay close attention to whitespace in this file cause it has 3 different definitions
(define-lex-abbrevs
  (alphanum (:/ "0" "9" "a" "z" "A" "Z"))
  (digit (:/ "0" "9"))
  (non-ascii (:~ (:/ #\u0 #\u9f)))
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
  (number (:or (:+ digit)
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
  (comment-without-closing-asterisk
   (:: "/*"
       (:* (:~ "*"))
       (:* (:: (:+ "*")
               (:~ (char-set "/*"))
               (:* (:~ "*"))))))
  (comment-without-closing-slash
   (:: comment-without-closing-asterisk (:+ "*")))
  (bad-comment (:or comment-without-closing-slash
                    comment-without-closing-asterisk))
  (uri-start (:: (:or "u" "U")
                 (:or "r" "R")
                 (:or "l" "L")
                 "(" whitespace))
  ; Covers both baduri1 and baduri2 from the standard
  (uri-without-closing-parenthesis
   (:: uri-start
       (:or string
            (:* (:or (char-set "!#$%&")
                     (:/ "*" "~")
                     non-ascii escape)))
       whitespace))
  (bad-uri (:or uri-without-closing-parenthesis
                (:: uri-start bad-string))))
; TODO attach lexemes
(define css-lexer
  (lexer
   [ident 'ident]
   [(:: "@" ident) 'at-keyword]
   [string 'string]
   [bad-string 'bad-string]
   [bad-uri 'bad-uri] ;
   [bad-comment 'bad-comment]
   [(:: "#" name) 'hash]
   [number 'number]
   [(:: number "%") 'percentage]
   [(:: number ident) 'dimension]
   [(:: uri-without-closing-parenthesis ")") 'uri]
   [(:: "u+"
        (:** 1 6 (:or alphanum "?"))
        (:? (:: "-" (:** 1 6 alphanum))))
    'unicode-range]
   ["<!--" 'cdo]
   ["-->" 'cdc]
   [":" 'colon]
   [";" 'semicolon]
   ["{" 'open-curly-bracket]
   ["}" 'close-curly-bracket]
   ["(" 'open-parentheses]
   [")" 'close-parentheses]
   ["[" 'open-square-bracket]
   ["]" 'close-square-bracket]
   [(:+ (char-set " \t\r\n\f")) 'whitespace] ; Different from the whitespace macro
   [(:: comment-without-closing-slash "/") 'comment]
   [(:: ident "(") 'function]
   ["~=" 'includes]
   ["|=" 'dash-match]
   [any-char 'delim]))
(define (make-tokenizer port)
  (thunk (css-lexer port)))
(define (test x) (apply-tokenizer-maker make-tokenizer x))
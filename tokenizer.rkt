#lang racket
(require br-parser-tools/lex)
(require (prefix-in : br-parser-tools/lex-sre))
(require brag/support)
; Pay close attention to whitespace in this file cause it has 3 different definitions
; Some lexer abbreviations could be made DRYer by moving them directly into css-lexer,
; but that would make the code too different from the standard
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
  (double-quote-start (:: #\"
                          (:* (:or (:~ (char-set "\n\r\f\\\""))
                                   (:: #\\ new-line)
                                   escape))))
  (single-quote-start (:: #\'
                          (:* (:or (:~ (char-set "\n\r\f\\\'"))
                                   (:: #\\ new-line)
                                   escape))))
  (string (:or (:: double-quote-start #\")
               (:: single-quote-start #\')))
  (bad-string (:: (:or double-quote-start single-quote-start) (:? #\\)))
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
(define (trim start str end)
  (substring str start (- (string-length str) end)))
(define (make-tokenizer port)
  (define (next-token)
    (define css-lexer
      (lexer
       [ident (token 'IDENT lexeme)]
       [(:: "@" ident) (token 'AT_KEYWORD (trim 1 lexeme 0))]
       [string (token 'STRING (trim 1 lexeme 1))]
       [bad-string (token 'BAD-STRING)]
       [bad-uri (token 'BAD-URI)]
       [bad-comment (token 'BAD-COMMENT)] ; Consumes the rest of the input
       [(:: "#" name) (token 'HASH (trim 1 lexeme 0))]
       [number (token 'NUMBER lexeme)]
       [(:: number "%") (token 'PERCENTAGE (trim 0 lexeme 1))]
       [(:: number ident) (token 'DIMENSION lexeme)]
       [(:: uri-without-closing-parenthesis ")") (token 'URI (trim 4 lexeme 1))]
       [(:: "u+"
            (:** 1 6 (:or alphanum "?"))
            (:? (:: "-" (:** 1 6 alphanum))))
         (token 'UNICODE-RANGE (trim 2 lexeme 0))]
       ["<!--" (token 'HTML-COMMENT-OPEN)]
       ["-->" (token 'HTML-COMMENT-CLOSE)]
       [":" (token 'COLON)]
       [";" (token 'SEMICOLON)]
       ["{" (token 'CURLY-BRACKET-OPEN)]
       ["}" (token 'CURLY-BRACKET-CLOSE)]
       ["(" (token 'PARENTHESIS-OPEN)]
       [")" (token 'PARENTHESIS-CLOSE)]
       ["[" (token 'SQUARE-BRACKET-OPEN)]
       ["]" (token 'SQUARE-BRACKET-CLOSE)]
       [(:+ (char-set " \t\r\n\f")) (token 'WHITESPACE)] ; Different from the whitespace in define-lex-abbrevs
       [(:: comment-without-closing-slash "/") (next-token)] ; Skip comment tokens
       [(:: ident "(") (token 'FUNCTION (trim 0 lexeme 1))]
       ["~=" (token 'INCLUDES)]
       ["|=" (token 'DASH-MATCH)]
       [any-char (token 'DELIMETER lexeme)]))
    (css-lexer port))
  next-token)
(define (tokenize x) (apply-tokenizer-maker make-tokenizer x))
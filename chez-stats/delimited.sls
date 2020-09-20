(library (chez-stats delimited)
  (export
   read-delim
   write-delim)

  (import (chezscheme))

  ;; READ ###################################################
  
  (define read-delim
    (case-lambda
      [(path) (read-delim-helper path #\, +inf.0)]
      [(path sep-char) (read-delim-helper path sep-char +inf.0)]
      [(path sep-char max-rows) (read-delim-helper path sep-char max-rows)]))

  ;; opting for 'comma and 'tab instead of passing
  ;; characters #\, and #\tab directly 
  (define (read-delim-helper path sep-char max-rows)
    (let ([p (open-input-file path)])
      (let loop ([row (read-line p)]
		 [results '()]
		 [iter max-rows])
	(cond [(or (eof-object? row) (< iter 1))
	       (close-port p)
	       (reverse results)]
	      [else
	       (loop (read-line p)
		     (cons (parse-line row sep-char) results)
		     (sub1 iter))]))))
  
  ;; https://stackoverflow.com/questions/37858083/how-to-read-a-line-of-input-in-chez-scheme
  (define (read-line . port)
    (define (eat p c)
      (if (and (not (eof-object? (peek-char p)))
	       (char=? (peek-char p) c))
	  (read-char p)))
    (let ([p (if (null? port) (current-input-port) (car port))])
      (let loop ([c (read-char p)]
		 [line '()])
	(cond [(eof-object? c) (if (null? line) c (list->string (reverse line)))]
	      [(char=? #\newline c) (eat p #\return) (list->string (reverse line))]
	      [(char=? #\return c) (eat p #\newline) (list->string (reverse line))]
	      [else (loop (read-char p) (cons c line))]))))

  ;; https://github.com/alex-hhh/data-frame/blob/master/private/csv.rkt
  (define (parse-line line sep-char)
    (let ([in (open-input-string line)])
      (let loop ([c (read-char in)]
		 [current ""]
		 [row '()]
		 [in-string #f])
	(cond [(eof-object? c)
	       (reverse (cons current row))]
	      [(and (char=? c sep-char) (not in-string))
	       (loop (read-char in) "" (cons current row) #f)]
	      [(and in-string (char=? c #\") (equal? (peek-char in) #\"))
	       (read-char in)             ; consume the next char
	       (loop (read-char in) (string-append current (string c)) row in-string)]
	      [(char=? c #\")
	       (loop (read-char in) (string-append current (string c)) row (not in-string))]
	      [else
	       (loop (read-char in) (string-append current (string c)) row in-string)]))))

  ;; WRITE ###################################################

  (define write-delim
    (case-lambda
      [(lst path) (write-delim-helper lst path #\, #t)]
      [(lst path sep-char) (write-delim-helper lst path sep-char #t)]
      [(lst path sep-char overwrite) (write-delim-helper lst path sep-char overwrite)]))

  (define (write-delim-helper lst path sep-char overwrite)
    (when (and (file-exists? path) (not overwrite))
      (assertion-violation path "file already exists"))
    (delete-file path)
    (let ([p (open-output-file path)])
      (let loop ([lst-local lst])
	(cond [(null? lst-local)
	       (close-port p)]
	      [else
	       (put-string p (delimit-list (car lst-local) sep-char))
	       (newline p)
	       (loop (cdr lst-local))]))))

  (define (delimit-list lst sep-char)
    (let loop ([lst lst]
	       [result ""]
	       [first? #t])
      (if (null? lst)
  	  result
  	  (let* ([item (car lst)]
  		 [sep-str (if first? "" (string sep-char))]
  		 [item-new (cond [(char? item) (string item)]
  				 [(symbol? item) (symbol->string item)]
  				 [(real? item) (number->string
  						(if (exact? item)
  						    (exact->inexact item)
  						    item))]
  				 [else (quote-string item sep-char)])])
  	    (loop (cdr lst) (string-append result sep-str item-new) #f)))))

  (define (quote-string str sep-char)
    (let* ([in (open-input-string str)]
	   [str-list (string->list str)]
	   [str-length (length str-list)])
      (if (not (or (member sep-char str-list) (member #\" str-list)))
  	  str  ;; return string unchanged b/c no commas or double quotes
  	  (let loop ([c (read-char in)]
  		     [result "\""]
		     [ctr 0])
  	    (cond [(eof-object? c)
  		   (string-append result "\"")]
		  [(and (char=? c #\") (or (= ctr 0) (= ctr (sub1 str-length))))
		   ;; don't add double-quote character to string
		   ;; when it is at start or end of string
		   (loop (read-char in) (string-append result "") (add1 ctr))]
		  ;; 2x double-quotes for double-quotes inside string (not at start or end)
  		  [(char=? c #\")
  		   (loop (read-char in) (string-append result "\"\"") (add1 ctr))]
  		  [else
  		   (loop (read-char in) (string-append result (string c)) (add1 ctr))])))))
  
  )


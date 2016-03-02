#lang racket
(define (date-handler file-in file-out)
  (define in (open-input-file file-in))
  (define out (open-output-file file-out #:exists 'replace))
  
  (define (converting-date string)
    (define dot-split (string-split string "."))
    (define slash-split (string-split string "/"))
    (define backslash-split (string-split string "\\"))
    (define space-split (string-split string))
    
    (define (month-handler string)
      (if (string? string)
          (cond [(equal? string "января") "01"][(equal? string "мая") "05"][(equal? string "сентября") "09"]
                [(equal? string "февраля") "02"][(equal? string "июня") "06"][(equal? string "октября") "10"]
                [(equal? string "марта") "03"][(equal? string "июля") "07"][(equal? string "ноября") "11"]
                [(equal? string "апреля") "04"][(equal? string "августа") "08"][(equal? string "декабря") "12"]
                [else string])
          string))
    
    (cond [(> (length dot-split) 1) string]
          [(> (length backslash-split) 1) (string-join backslash-split ".")]
          [(> (length slash-split) 1) (string-join slash-split ".")]
          [else (string-join (map month-handler space-split) ".")]))
  
  (define (iter)
    (define line (read-line in))
      (if (equal? line eof) (close-output-port out)
      (begin (display (converting-date line) out)
             (display "\n" out)(iter))))
  (iter))
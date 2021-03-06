#lang racket
(require 2htdp/batch-io)
(define (file-merging directory file-out)
  (define lst (directory-list directory))
  (define out (open-output-file file-out #:exists 'replace))
  (for-each (λ(file)
              (let ((in (open-input-file file)))
                (for-each (λ(line)
                            ((display line out)
                             (display #\newline out)))
                          (read-lines in))))
            lst))
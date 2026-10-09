#lang racket

(require "thing.rkt")

(provide read-thing-log
         append-thing-log!)

;; Logs live beside the persisted things. Keeping them in their own directory
;; lets existing serialized user and area records load without a schema change.
(define (log-file log-path this-thing)
  (build-path log-path (thing-id this-thing)))

(define (read-thing-log log-path this-thing)
  (define path (log-file log-path this-thing))
  (if (file-exists? path)
      (let ([entries (call-with-input-file path read)])
        (unless (and (list? entries) (andmap string? entries))
          (error 'read-thing-log "Invalid log for ~a" (thing-id this-thing)))
        entries)
      '()))

(define (append-thing-log! log-path this-thing entry)
  (unless (and (string? entry) (not (string=? (string-trim entry) "")))
    (raise-argument-error 'append-thing-log! "nonempty string" entry))
  (make-directory* log-path)
  (define entries (append (read-thing-log log-path this-thing)
                          (list (string-trim entry))))
  (call-with-output-file (log-file log-path this-thing)
    (λ (out) (write entries out))
    #:exists 'replace)
  entries)

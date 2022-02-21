#lang racket/base

(module+ test
  (require rackunit))

(require crypto)

(provide (struct-out outpoint)
         (struct-out input)
         (struct-out output)
         (struct-out transaction))


(struct outpoint (transaction-hash index) #:transparent)

(struct input (script witness sequence prevout) #:transparent)
(struct output (script value) #:transparent)
(struct transaction (version-number flag inputs outputs witnesses lock-time) #:transparent)

;; (module+ test
;; (test-case
;;     "transaction serialization for signing"
;;   (let* ([test-prevout (outpoint "transaction hash" 0)]
;;          [test-inputs (list (input '("script" "elements") '() 1 test-prevout))]
;;          [test-outputs (list (output '("script" "elements") 100))]
;;          [test-transaction (transaction 1 0 test-inputs test-outputs '() '())])
;;     (check-not-false (digest-transaction-inputs test-transaction 0))
;;     (check-false (digest-transaction-inputs test-transaction 1)))))

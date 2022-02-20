#lang racket/base

(module+ test
  (require rackunit))

(require crypto)

(provide (struct-out outpoint)
         (struct-out input)
         (struct-out output)
         (struct-out transaction)
         transaction-sign)


(struct outpoint (transaction-hash index) #:transparent)

(struct input (script witness sequence prevout) #:transparent)
(struct output (script value) #:transparent)
(struct transaction (version-number flag inputs outputs witnesses lock-time) #:transparent)

;; Ignore sighash for now, assume sighash all
(define (transaction-sign key tx [sighash 'all])
  (let ([digest (transaction-digest-for-sign tx sighash)])
    (pk-sign key digest)))

;; (module+ test
;; (test-case
;;     "transaction serialization for signing"
;;   (let* ([test-prevout (outpoint "transaction hash" 0)]
;;          [test-inputs (list (input '("script" "elements") '() 1 test-prevout))]
;;          [test-outputs (list (output '("script" "elements") 100))]
;;          [test-transaction (transaction 1 0 test-inputs test-outputs '() '())])
;;     (check-not-false (digest-transaction-inputs test-transaction 0))
;;     (check-false (digest-transaction-inputs test-transaction 1)))))

;; Ignore sighash for now, assume sighash all
;; https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki
(define (transaction-digest-for-sign tx [sighash 'all])
  (let ([signed #t]
        [unsigned #f]
        [big-endian #t]
        [little-endian #f])
    (define (digest-transaction-version tx)
      (integer->integer-bytes (transaction-version-number tx) 4 unsigned little-endian))
    (bytes-append (digest-transaction-version tx)
                  (transaction-lock-time tx)
                  (transaction-inputs tx)
                  (transaction-outputs tx))))

(define-module (store colors)
  #:export (set-color-hex!
            get-color-hex
            get-color-rgb))

(define hs (make-hash-table))

(define (set-color-hex! key value)
  (hashq-set! hs key value))

(define (get-color-hex key)
  (hashq-ref hs key "#000000"))

(define (hex->rgb hex)
  (let* ((r (string-take (string-drop hex 1) 2))
         (g (string-take (string-drop hex 3) 2))
         (b (string-take (string-drop hex 5) 2)))
    (map (λ (c) (/ (string->number c 16) 255)) (list r g b))))

(define (get-color-rgb key)
  (hex->rgb (get-color-hex key)))

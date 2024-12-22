(use-modules (cairo)
             (lib cairo)
             (lib date)
             (templates agendamento3x3))

(define page A5)

(define s (cairo-pdf-surface-create (width page) (height page) "test.pdf"))
(define cr (cairo-create s))

(agendamento3x3 cr #:page page #:from (make-date-ymd 2025 1) #:to (make-date-ymd 2025 12 31))

(cairo-destroy cr)
(cairo-surface-destroy s)

(use-modules (oop goops)
             (cairo)
             (cairo-table))

(define (mm-to-points mm)
  (* 72 (/ mm 25.4)))

(define page-width (mm-to-points 150))
(define page-height (mm-to-points 210))
(define margin (mm-to-points 10))

(define (set-source-header-bg cr)
  (cairo-set-source-rgb cr 0.98 0.50 0.45))

(define (set-source-header-text cr)
  (cairo-set-source-rgb cr 1 1 1))

(define (set-source-altrow-bg cr)
  (cairo-set-source-rgb cr 0.94 0.90 0.55))

(define (set-line-width-table cr)
  (cairo-set-line-width cr 0.5))

(define (set-source-table-border cr)
  (cairo-set-source-rgb cr 0.55 0 0))




(define s (cairo-pdf-surface-create page-width page-height "test.pdf"))
(define cr (cairo-create s))

(define table (make <cairo-table> #:cairo cr
                    #:x margin #:y margin
                    #:width (- page-width (* 2 margin))
                    #:height (- page-height (* 2 margin))
                    #:rows 31 #:columns 3
                    #:cell-padding-x (mm-to-points 1)))

;; header bg
(set-source-header-bg cr)
(area-to-rectangle (cell-area table 0 0 1 (columns table)))
(cairo-fill cr)

;; alternating rows bg
(set-source-altrow-bg cr)
(let loop ((row 1))
  (when (< row (rows table))
    (area-to-rectangle (cell-area table row 0 1 (columns table)))
    (cairo-fill cr)
    (loop (+ row 3))))

;; borders
(set-line-width-table cr)
(set-source-table-border cr)
(for-each (lambda (row)
            ;; (area-to-rectangle (cell-area table row 0 1 1))
            ;; (cairo-stroke cr)
            ;; (area-to-rectangle (cell-area table row 1 1 2))
            ;; (cairo-stroke cr))
            (for-each (lambda (col)
                        (area-to-rectangle (cell-area table row col 1 1))
                        (cairo-stroke cr))
                      (iota (columns table))))
          (iota (rows table)))

;; text
(set-source-table-border cr)
(show-text-centered (cell-inner-area table 0 0 1 1) "Segunda")
(show-text-right (cell-inner-area table 0 0 1 1) "12")
(show-text-centered (cell-inner-area table 0 1 1 1) "Terça")
(show-text-right (cell-inner-area table 0 1 1 1) "5")

(define (show-time-rows col)
  (for-each (lambda (r)
              (let* ((hour (string-pad (number->string (+ r 8)) 2 #\0))
                     (time (string-append hour ":00"))
                     (row (1+ (* r 3)))
                     (area (cell-area table row col 1 1)))
                (show-text-centered area time)))
            (iota 10)))

(set-source-table-border cr)
(show-time-rows 0)
(show-time-rows 1)

;; TODO use SRFI-19 for date/time ops

(cairo-destroy cr)
(cairo-surface-destroy s)

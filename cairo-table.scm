(define-module (cairo-table)
  #:use-module (oop goops)
  #:use-module (cairo)
  #:export (<cairo-area>
            <cairo-table>
            get-cairo set-cairo!
            x x!
            y y!
            width width!
            height height!
            rows rows!
            columns columns!
            cell-padding-x cell-padding-x!
            cell-area
            cell-inner-area
            area-to-rectangle
            move-to-area-center
            show-text-centered
            show-text-right))

(define-class <cairo-area> ()
  (cairo #:getter get-cairo #:setter set-cairo! #:init-keyword #:cairo)
  (x #:init-value 0 #:getter x #:setter x! #:init-keyword #:x)
  (y #:init-value 0 #:getter y #:setter y! #:init-keyword #:y)
  (width #:init-value 0 #:getter width #:setter width! #:init-keyword #:width)
  (height #:init-value 0 #:getter height #:setter height! #:init-keyword #:height))

(define-class <cairo-table> (<cairo-area>)
  (rows #:init-value 1 #:getter rows #:setter rows! #:init-keyword #:rows)
  (columns #:init-value 1 #:getter columns #:setter columns! #:init-keyword #:columns)
  (cell-padding-x #:init-value 0 #:getter cell-padding-x #:setter cell-padding-x! #:init-keyword #:cell-padding-x))

(define-method (cell-area (table <cairo-table>) row col rowspan colspan)
  (let ((col-width (/ (width table) (columns table)))
        (row-height (/ (height table) (rows table))))
    (make <cairo-area>
      #:cairo (get-cairo table)
      #:x (+ (x table) (* col col-width))
      #:y (+ (y table) (* row row-height))
      #:width (* colspan col-width)
      #:height (* rowspan row-height))))

(define-method (cell-inner-area (table <cairo-table>) row col rowspan colspan)
  (let ((col-width (/ (width table) (columns table)))
        (row-height (/ (height table) (rows table))))
    (make <cairo-area>
      #:cairo (get-cairo table)
      #:x (+ (x table) (* col col-width) (cell-padding-x table))
      #:y (+ (y table) (* row row-height))
      #:width (- (* colspan col-width) (* 2 (cell-padding-x table)))
      #:height (* rowspan row-height))))

(define-method (area-to-rectangle (area <cairo-area>))
  (cairo-rectangle (get-cairo area)
                   (x area)
                   (y area)
                   (width area)
                   (height area)))

(define-method (move-to-area-center (area <cairo-area>))
  (cairo-move-to (get-cairo area)
                 (+ (x area) (/ (width area) 2))
                 (+ (y area) (/ (height area) 2))))

(define-method (show-text-centered (area <cairo-area>) text)
  (define ex (cairo-text-extents (get-cairo area) text))
  (let ((h (cairo-text-extents:height ex))
        (w (cairo-text-extents:width ex))
        (bx (cairo-text-extents:x-bearing ex))
        (by (cairo-text-extents:y-bearing ex)))
    (move-to-area-center area)
    (cairo-rel-move-to (get-cairo area)
                       (- (+ bx (/ w 2)))
                       ;; (- (+ by (/ h 2)))
                       (- (/ by 2)))
    (cairo-show-text (get-cairo area) text)))

(define-method (show-text-right (area <cairo-area>) text)
  (define ex (cairo-text-extents (get-cairo area) text))
  (let ((h (cairo-text-extents:height ex))
        (w (cairo-text-extents:width ex))
        (ax (cairo-text-extents:x-advance ex))
        (by (cairo-text-extents:y-bearing ex)))
    (move-to-area-center area)
    (cairo-rel-move-to (get-cairo area)
                       (- (/ (width area) 2) ax)
                       (- (/ by 2)))
    (cairo-show-text (get-cairo area) text)))

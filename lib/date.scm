(define-module (lib date)
  #:use-module (srfi srfi-19)
  #:export (day-generator
            date->list
            make-date-ymd
            sunday?
            monday?
            tuesday?
            wednesday?
            thursday?
            friday?
            saturday?
            weekend?
            business-day?))

(define* (make-date-ymd year #:optional (month 1) (day 1))
  (make-date 0 0 0 0 day month year 0))

(define one-day (make-time time-duration 0 (* 24 60 60)))

(define *eof-object* (read (open-input-string "")))

(define (day-generator from to)
  (define next (date->time-utc from))
  (define end-time (date->time-utc to))
  (lambda ()
    (if (time>? next end-time) *eof-object*
        (let ((current next))
          (set! next (add-duration next one-day))
          (time-utc->date current 0)))))

(define (date->list from to)
  (let ((get-next-date (day-generator from to)))
    (let loop ((date (get-next-date)) (lst '()))
      (if (eof-object? date) (reverse lst)
          (loop (get-next-date) (cons date lst))))))

(define (sunday? date)
  (= 0 (date-week-day date)))

(define (monday? date)
  (= 1 (date-week-day date)))

(define (tuesday? date)
  (= 2 (date-week-day date)))

(define (wednesday? date)
  (= 3 (date-week-day date)))

(define (thursday? date)
  (= 4 (date-week-day date)))

(define (friday? date)
  (= 5 (date-week-day date)))

(define (saturday? date)
  (= 6 (date-week-day date)))

(define (weekend? date)
  (or (sunday? date) (saturday? date)))

(define business-day? (negate weekend?))

(use-modules (ice-9 popen)
             (ice-9 rdelim))

;; absolute date, represented by a year, month (1-12), and day (1-31)
(define (date y m d)
  (list y m d))
(define (date:year date)
  (car date))
(define (date:month date)
  (cadr date))
(define (date:day date)
  (caddr date))

;; get the difference between two dates
(define (ddiff d1 d0)
  (define (days-in-month m)
    (list-ref '(31
                28  ;; TODO: handle leap year correctly
                31
                30
                31
                30
                31
                31
                30
                31
                30
                31)
              (- m 1)))
  (define (previous-month m)
    (if (= m 1)
        12
        (- m 1)))
  (define (normalize-months y m d)
    (if (< m 0)
        (normalize-months (- y 1) (+ 12 m) d)
        (list y m d)))
  (define (normalize-days y m d d1 d0)
    (if (< d 0)
        (let* ((start-m (previous-month (date:month d1)))
               (new-days (+ d (days-in-month start-m))))
          (if (< new-days 0)
              ;; note that days can wrap twice around february (for
              ;; example), but can never wrap 3x
              (normalize-months
               y
               (- m 2)
               (+ new-days
                  (days-in-month (previous-month start-m))))
              (normalize-months
               y
               (- m 1)
               (+ d (days-in-month start-m)))))
        (normalize-months y m d)))
  (let ((years (- (date:year d1) (date:year d0)))
        (months (- (date:month d1) (date:month d0)))
        (days (- (date:day d1) (date:day d0))))
    (normalize-days years months days d1 d0)))
(define (ddiff:years diff)
  (car diff))
(define (ddiff:months diff)
  (cadr diff))
(define (ddiff:days diff)
  (caddr diff))

(define skydio-start-date (date 2019 1 22))

(define config-file
  (let ((home (getenv "HOME")))
    (string-append home "/.i3/status")))

(define (format-uptime)
  (let* ((tm-now (localtime (current-time)))
         (now (date (+ 1900 (tm:year tm-now))
                    (+ 1 (tm:mon tm-now))
                    (tm:mday tm-now)))
         (diff (ddiff now skydio-start-date)))
    (string-append (number->string (ddiff:years diff)) "y "
                   (number->string (ddiff:months diff)) "m "
                   (number->string (ddiff:days diff)) "d")))

(let ((i3status (open-pipe* OPEN_READ "i3status" "-c" config-file)))
  (define (do-loop)
    (let ((status-line (read-line i3status))
          (uptime (format-uptime)))
      (display
       (string-append status-line " (" uptime ")"))
      (newline)
      ;; guile uses block buffering for non-tty ports; force it to
      ;; flush here so i3 can read the new status line
      (force-output)
      (do-loop)))
  (do-loop))

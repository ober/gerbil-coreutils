(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports)

(def (main . args)
  (parameterize ((program-name "nl"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((files .rest)
                (body-style (or .?body-numbering "t"))
                (width (string->number (or .?number-width "6")))
                (sep (or .?number-separator "\t"))
                (start (string->number (or .?starting-line-number "1")))
                (incr (string->number (or .?line-increment "1")))
                (format-type (or .?number-format "rn"))
                (blank-join (string->number (or .?join-blank-lines "1"))))
            (let ((line-num start)
                  (blank-count 0))
              (process-nl-files
                (if (null? files) '("-") files)
                (lambda (port)
                  (let loop ()
                    (let (line (read-line port))
                      (unless (eof-object? line)
                        (let* ((blank? (string=? line ""))
                               (number? (should-number? body-style line blank?
                                                         blank-count blank-join)))
                          (if blank? (set! blank-count (+ blank-count 1))
                            (set! blank-count 0))
                          (if number?
                            (begin
                              (display (format-number line-num width format-type))
                              (display sep)
                              (displayln line)
                              (set! line-num (+ line-num incr)))
                            (begin
                              (display (make-string width #\space))
                              (display sep)
                              (displayln line))))
                        (loop))))))))))
      args
      program: "nl"
      help: "Write each FILE to standard output, with line numbers added."
      (option 'body-numbering "-b" "--body-numbering"
        help: "use STYLE for numbering body lines (a/t/n)" default: #f)
      (option 'number-width "-w" "--number-width"
        help: "use NUMBER columns for line numbers" default: #f)
      (option 'number-separator "-s" "--number-separator"
        help: "add STRING after (possible) line number" default: #f)
      (option 'starting-line-number "-v" "--starting-line-number"
        help: "first line number on each logical page" default: #f)
      (option 'line-increment "-i" "--line-increment"
        help: "line number increment at each line" default: #f)
      (option 'number-format "-n" "--number-format"
        help: "insert line numbers according to FORMAT (ln/rn/rz)" default: #f)
      (option 'join-blank-lines "-l" "--join-blank-lines"
        help: "group of NUMBER empty lines counted as one" default: #f)
      (rest-arguments 'rest))))

(def (process-nl-files files proc)
  (for-each
    (lambda (f)
      (if (equal? f "-")
        (proc (current-input-port))
        (with-catch
          (lambda (e) (warn "~a: No such file or directory" f))
          (lambda ()
            (let (port (open-input-file f))
              (try (proc port)
                (finally (close-input-port port))))))))
    files))

(def (should-number? style line blank? blank-count blank-join)
  (cond
    ((string=? style "a") #t)          ;; all lines
    ((string=? style "t")              ;; non-empty only
     (not blank?))
    ((string=? style "n") #f)          ;; no lines
    (else (not blank?))))              ;; default: non-empty

(def (format-number n width fmt)
  (let (s (number->string n))
    (cond
      ((string=? fmt "rn") ;; right justified
       (string-append (make-string (max 0 (- width (string-length s))) #\space) s))
      ((string=? fmt "ln") ;; left justified
       (string-append s (make-string (max 0 (- width (string-length s))) #\space)))
      ((string=? fmt "rz") ;; right justified, zero-filled
       (string-append (make-string (max 0 (- width (string-length s))) #\0) s))
      (else ;; default rn
       (string-append (make-string (max 0 (- width (string-length s))) #\space) s)))))

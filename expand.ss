(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports)

(def (main . args)
  (parameterize ((program-name "expand"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((files .rest)
                (initial-only? .?initial)
                (tabstop (parse-tabstop (or .?tabs "8"))))
            (let (files (if (null? files) '("-") files))
              (for-each
                (lambda (file)
                  (expand-file file initial-only? tabstop))
                files)))))
      args
      program: "expand"
      help: "Convert tabs in each FILE to spaces, writing to standard output."
      (flag 'initial "-i" "--initial"
        help: "do not convert tabs after non blanks")
      (option 'tabs "-t" "--tabs"
        help: "have tabs N characters apart, not 8" default: #f)
      (rest-arguments 'rest))))

(def (parse-tabstop str)
  (let (n (string->number str))
    (if n n 8)))

(def (expand-file file initial-only? tabstop)
  (let (proc
    (lambda (port)
      (let loop ((col 0) (initial? #t))
        (let (c (read-char port))
          (cond
            ((eof-object? c) (void))
            ((eqv? c #\newline)
             (newline)
             (loop 0 #t))
            ((and (eqv? c #\tab) (or (not initial-only?) initial?))
             (let* ((next-stop (* (+ (quotient col tabstop) 1) tabstop))
                    (spaces (- next-stop col)))
               (display (make-string spaces #\space))
               (loop next-stop initial?)))
            (else
              (write-char c)
              (loop (+ col 1) (and initial? (eqv? c #\space)))))))))
    (if (equal? file "-")
      (proc (current-input-port))
      (with-catch
        (lambda (e) (warn "~a: No such file or directory" file))
        (lambda ()
          (let (port (open-input-file file))
            (try (proc port)
              (finally (close-input-port port)))))))))

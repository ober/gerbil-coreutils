(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar)

(def (main . args)
  (parameterize ((program-name "printenv"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((vars .rest)
                (delim (if .?null #\nul #\newline)))
            (if (null? vars)
              ;; Print all environment variables
              (for-each
                (lambda (pair)
                  (display (car pair))
                  (display "=")
                  (display (cdr pair))
                  (write-char delim))
                (get-environment-variables))
              ;; Print specific variables
              (let (fail? #f)
                (for-each
                  (lambda (name)
                    (let (val (getenv name #f))
                      (if val
                        (begin (display val) (write-char delim))
                        (set! fail? #t))))
                  vars)
                (exit (if fail? 1 0)))))))
      args
      program: "printenv"
      help: "Print the values of the specified environment VARIABLE(s). If no VARIABLE is specified, print name and value pairs for them all."
      (flag 'null "-0" "--null"
        help: "end each output line with NUL, not newline")
      (rest-arguments 'rest help: "variable names"))))

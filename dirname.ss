(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar)

(def (main . args)
  (parameterize ((program-name "dirname"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((names .rest)
                (delim (if .?zero #\nul #\newline)))
            (when (null? names)
              (die "missing operand"))
            (for-each
              (lambda (name)
                (display (compute-dirname name))
                (write-char delim))
              names))))
      args
      program: "dirname"
      help: "Output each NAME with its last non-slash component and trailing slashes removed; if NAME contains no /'s, output '.' (meaning the current directory)."
      (flag 'zero "-z" "--zero"
        help: "end each output line with NUL, not newline")
      (rest-arguments 'rest))))

(def (compute-dirname name)
  ;; Remove trailing slashes
  (let* ((name (let loop ((n name))
                 (if (and (> (string-length n) 1)
                          (eqv? (string-ref n (- (string-length n) 1)) #\/))
                   (loop (substring n 0 (- (string-length n) 1)))
                   n)))
         (pos (string-last-index-of name #\/)))
    (cond
      ((not pos) ".")
      ((= pos 0) "/")
      (else
        ;; Remove trailing slashes from result
        (let loop ((end pos))
          (if (and (> end 1) (eqv? (string-ref name (- end 1)) #\/))
            (loop (- end 1))
            (substring name 0 end)))))))

(def (string-last-index-of str ch)
  (let loop ((i (- (string-length str) 1)))
    (cond
      ((< i 0) #f)
      ((eqv? (string-ref str i) ch) i)
      (else (loop (- i 1))))))

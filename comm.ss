(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports)

(def (main . args)
  (parameterize ((program-name "comm"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((files .rest)
                (suppress-1? .?suppress-1)
                (suppress-2? .?suppress-2)
                (suppress-3? .?suppress-3)
                (zero-term? .?zero-terminated)
                (out-delim (or .?output-delimiter "\t")))
            (when (not (= (length files) 2))
              (die "requires exactly two files"))
            (comm-files (car files) (cadr files)
                        suppress-1? suppress-2? suppress-3?
                        out-delim zero-term?))))
      args
      program: "comm"
      help: "Compare two sorted files line by line."
      (flag 'suppress-1 "-1" "-1"
        help: "suppress column 1 (lines unique to FILE1)")
      (flag 'suppress-2 "-2" "-2"
        help: "suppress column 2 (lines unique to FILE2)")
      (flag 'suppress-3 "-3" "-3"
        help: "suppress column 3 (lines that appear in both files)")
      (option 'output-delimiter "--output-delimiter" "--output-delimiter"
        help: "separate columns with STR" default: #f)
      (flag 'zero-terminated "-z" "--zero-terminated"
        help: "line delimiter is NUL, not newline")
      (rest-arguments 'rest))))

(def (open-comm-port file)
  (if (equal? file "-")
    (current-input-port)
    (open-input-file file)))

(def (comm-files file1 file2 suppress-1? suppress-2? suppress-3? out-delim zero-term?)
  (let ((port1 (open-comm-port file1))
        (port2 (open-comm-port file2))
        (line-delim (if zero-term? #\nul #\newline))
        (eol (if zero-term? "\0" "\n")))
    (let loop ((l1 (read-line port1 line-delim))
               (l2 (read-line port2 line-delim)))
      (cond
        ((and (eof-object? l1) (eof-object? l2))
         'done)
        ((eof-object? l1)
         ;; Rest of file2
         (unless suppress-2?
           (display (col2-prefix suppress-1? out-delim))
           (display l2)
           (display eol))
         (loop l1 (read-line port2 line-delim)))
        ((eof-object? l2)
         ;; Rest of file1
         (unless suppress-1?
           (display l1)
           (display eol))
         (loop (read-line port1 line-delim) l2))
        ((string<? l1 l2)
         (unless suppress-1?
           (display l1)
           (display eol))
         (loop (read-line port1 line-delim) l2))
        ((string<? l2 l1)
         (unless suppress-2?
           (display (col2-prefix suppress-1? out-delim))
           (display l2)
           (display eol))
         (loop l1 (read-line port2 line-delim)))
        (else
         ;; Equal
         (unless suppress-3?
           (display (col3-prefix suppress-1? suppress-2? out-delim))
           (display l1)
           (display eol))
         (loop (read-line port1 line-delim)
               (read-line port2 line-delim)))))
    (unless (equal? file1 "-") (close-input-port port1))
    (unless (equal? file2 "-") (close-input-port port2))))

(def (col2-prefix suppress-1? delim)
  (if suppress-1? "" delim))

(def (col3-prefix suppress-1? suppress-2? delim)
  (string-append
    (if suppress-1? "" delim)
    (if suppress-2? "" delim)))

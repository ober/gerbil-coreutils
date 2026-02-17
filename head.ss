(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports)

(def (main . args)
  (parameterize ((program-name "head"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((files .rest)
                (count (or .?lines "10"))
                (byte-count .?bytes)
                (quiet? .?quiet)
                (verbose? .?verbose))
            (let* ((files (if (null? files) '("-") files))
                   (multi? (> (length files) 1)))
              (for-each
                (lambda (file)
                  (when (or verbose? (and multi? (not quiet?)))
                    (displayln "==> " (if (equal? file "-") "standard input" file) " <=="))
                  (if byte-count
                    (head-bytes file (parse-head-count byte-count))
                    (head-lines file (parse-head-count count))))
                files)))))
      args
      program: "head"
      help: "Print the first 10 lines of each FILE to standard output."
      (option 'lines "-n" "--lines"
        help: "print the first NUM lines instead of the first 10"
        default: #f)
      (option 'bytes "-c" "--bytes"
        help: "print the first NUM bytes"
        default: #f)
      (flag 'quiet "-q" "--quiet"
        help: "never print headers giving file names")
      (flag 'verbose "-v" "--verbose"
        help: "always print headers giving file names")
      (rest-arguments 'rest))))

(def (parse-head-count str)
  (let* ((s (string->number str)))
    (if s s
      (begin (die "invalid number of lines: '~a'" str) 0))))

(def (head-lines file n)
  (let (proc
    (lambda (port)
      (let loop ((i 0))
        (when (< i n)
          (let (line (read-line port))
            (unless (eof-object? line)
              (displayln line)
              (loop (+ i 1))))))))
    (if (equal? file "-")
      (proc (current-input-port))
      (with-catch
        (lambda (e) (warn "cannot open '~a' for reading: No such file or directory" file))
        (lambda ()
          (let (port (open-input-file file))
            (try (proc port)
              (finally (close-input-port port)))))))))

(def (head-bytes file n)
  (let (proc
    (lambda (port)
      (let loop ((i 0))
        (when (< i n)
          (let (c (read-char port))
            (unless (eof-object? c)
              (write-char c)
              (loop (+ i 1))))))))
    (if (equal? file "-")
      (proc (current-input-port))
      (with-catch
        (lambda (e) (warn "cannot open '~a' for reading: No such file or directory" file))
        (lambda ()
          (let (port (open-input-file file))
            (try (proc port)
              (finally (close-input-port port)))))))))

(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports)

(def (main . args)
  (parameterize ((program-name "tail"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((files .rest)
                (count-str (or .?lines "10"))
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
                    (tail-bytes file (parse-tail-count byte-count))
                    (tail-lines file (parse-tail-count count-str))))
                files)))))
      args
      program: "tail"
      help: "Print the last 10 lines of each FILE to standard output."
      (option 'lines "-n" "--lines"
        help: "output the last NUM lines instead of the last 10"
        default: #f)
      (option 'bytes "-c" "--bytes"
        help: "output the last NUM bytes"
        default: #f)
      (flag 'quiet "-q" "--quiet"
        help: "never output headers giving file names")
      (flag 'verbose "-v" "--verbose"
        help: "always output headers giving file names")
      (rest-arguments 'rest))))

(def (parse-tail-count str)
  ;; +N means start from line N (1-indexed)
  ;; -N or N means last N lines
  (cond
    ((and (> (string-length str) 0)
          (eqv? (string-ref str 0) #\+))
     (let (n (string->number (substring str 1 (string-length str))))
       (if n (cons 'from n)
         (begin (die "invalid number of lines: '~a'" str) (cons 'last 10)))))
    (else
      (let* ((s (if (and (> (string-length str) 0)
                         (eqv? (string-ref str 0) #\-))
                  (substring str 1 (string-length str))
                  str))
             (n (string->number s)))
        (if n (cons 'last n)
          (begin (die "invalid number of lines: '~a'" str) (cons 'last 10)))))))

(def (tail-lines file spec)
  (let (proc
    (lambda (port)
      (case (car spec)
        ((from)
         ;; Start from line N (1-indexed)
         (let (start (cdr spec))
           (let skip ((i 1))
             (when (< i start)
               (let (line (read-line port))
                 (unless (eof-object? line)
                   (skip (+ i 1))))))
           (let loop ()
             (let (line (read-line port))
               (unless (eof-object? line)
                 (displayln line)
                 (loop))))))
        ((last)
         ;; Last N lines - use a circular buffer
         (let* ((n (cdr spec))
                (buf (make-vector n #f))
                (count 0))
           (let loop ()
             (let (line (read-line port))
               (unless (eof-object? line)
                 (vector-set! buf (modulo count n) line)
                 (set! count (+ count 1))
                 (loop))))
           ;; Output
           (let* ((total (min count n))
                  (start (if (< count n) 0 (modulo count n))))
             (let loop ((i 0))
               (when (< i total)
                 (displayln (vector-ref buf (modulo (+ start i) n)))
                 (loop (+ i 1))))))))))
    (if (equal? file "-")
      (proc (current-input-port))
      (with-catch
        (lambda (e) (warn "cannot open '~a' for reading: No such file or directory" file))
        (lambda ()
          (let (port (open-input-file file))
            (try (proc port)
              (finally (close-input-port port)))))))))

(def (tail-bytes file spec)
  (let (proc
    (lambda (port)
      (let* ((content (read-all-as-string port))
             (len (string-length content)))
        (case (car spec)
          ((from)
           (let (start (min (- (cdr spec) 1) len))
             (display (substring content start len))))
          ((last)
           (let (start (max 0 (- len (cdr spec))))
             (display (substring content start len))))))))
    (if (equal? file "-")
      (proc (current-input-port))
      (with-catch
        (lambda (e) (warn "cannot open '~a' for reading: No such file or directory" file))
        (lambda ()
          (let (port (open-input-file file))
            (try (proc port)
              (finally (close-input-port port)))))))))

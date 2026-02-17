(export
  process-files
  for-each-line
  read-line/delim
  write-line/delim
  safe-open-input-file)

(import :gerbil-coreutils/common
        :std/format)

;; Open a file with error handling; returns port or calls die
(def (safe-open-input-file path)
  (with-catch
    (lambda (e)
      (die "~a: No such file or directory" path))
    (lambda ()
      (open-input-file path))))

;; Process a list of files (or stdin if empty/"-")
;; proc is called with an input port for each file
(def (process-files files proc)
  (if (or (null? files) (equal? files '("-")))
    (proc (current-input-port))
    (for-each
      (lambda (f)
        (if (equal? f "-")
          (proc (current-input-port))
          (let (port (safe-open-input-file f))
            (try
              (proc port)
              (finally
                (close-input-port port))))))
      files)))

;; Call proc on each line read from port
(def (for-each-line port proc)
  (let loop ()
    (let (line (read-line port))
      (unless (eof-object? line)
        (proc line)
        (loop)))))

;; Read until delimiter character (for NUL support)
(def (read-line/delim port delim)
  (if (eqv? delim #\newline)
    (read-line port)
    (let (out (open-output-string))
      (let loop ()
        (let (c (read-char port))
          (cond
            ((eof-object? c)
             (let (s (get-output-string out))
               (if (string=? s "") c s)))
            ((eqv? c delim)
             (get-output-string out))
            (else
              (write-char c out)
              (loop))))))))

;; Write line with delimiter
(def (write-line/delim line delim)
  (display line)
  (write-char delim))

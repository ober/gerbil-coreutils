(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports)

(def (main . args)
  (parameterize ((program-name "paste"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((files .rest)
                (delimiters (parse-delimiters (or .?delimiters "\t")))
                (serial? .?serial)
                (zero-term? .?zero-terminated))
            (let (files (if (null? files) '("-") files))
              (if serial?
                (paste-serial files delimiters zero-term?)
                (paste-parallel files delimiters zero-term?))))))
      args
      program: "paste"
      help: "Merge lines of files."
      (option 'delimiters "-d" "--delimiters"
        help: "reuse characters from LIST instead of TABs" default: #f)
      (flag 'serial "-s" "--serial"
        help: "paste one file at a time instead of in parallel")
      (flag 'zero-terminated "-z" "--zero-terminated"
        help: "line delimiter is NUL, not newline")
      (rest-arguments 'rest))))

(def (parse-delimiters str)
  (let ((len (string-length str)))
    (let loop ((i 0) (acc '()))
      (if (>= i len)
        (list->string (reverse acc))
        (let (c (string-ref str i))
          (if (and (eqv? c #\\) (< (+ i 1) len))
            (let (next (string-ref str (+ i 1)))
              (case next
                ((#\n) (loop (+ i 2) (cons #\newline acc)))
                ((#\t) (loop (+ i 2) (cons #\tab acc)))
                ((#\0) (loop (+ i 2) (cons #\nul acc)))
                ((#\\) (loop (+ i 2) (cons #\\ acc)))
                (else (loop (+ i 2) (cons next acc)))))
            (loop (+ i 1) (cons c acc))))))))

(def (open-paste-port file)
  (if (equal? file "-")
    (current-input-port)
    (open-input-file file)))

(def (paste-parallel files delimiters zero-term?)
  (let* ((ports (map open-paste-port files))
         (ndelims (string-length delimiters))
         (line-delim (if zero-term? #\nul #\newline)))
    (let loop ()
      (let* ((lines (map (lambda (p) (read-line p line-delim)) ports))
             (all-eof? (every eof-object? lines)))
        (unless all-eof?
          (let lp ((ls lines) (idx 0) (first? #t))
            (unless (null? ls)
              (unless first?
                (write-char (string-ref delimiters (modulo (- idx 1) ndelims))))
              (let (line (car ls))
                (unless (eof-object? line)
                  (display line)))
              (lp (cdr ls) (+ idx 1) #f)))
          (display (if zero-term? "\0" "\n"))
          (loop))))
    ;; Close non-stdin ports
    (for-each (lambda (f p)
                (unless (equal? f "-") (close-input-port p)))
              files ports)))

(def (every pred lst)
  (cond
    ((null? lst) #t)
    ((pred (car lst)) (every pred (cdr lst)))
    (else #f)))

(def (paste-serial files delimiters zero-term?)
  (let ((ndelims (string-length delimiters))
        (line-delim (if zero-term? #\nul #\newline)))
    (for-each
      (lambda (file)
        (let (port (open-paste-port file))
          (let loop ((first? #t) (delim-idx 0))
            (let (line (read-line port line-delim))
              (unless (eof-object? line)
                (unless first?
                  (write-char (string-ref delimiters (modulo delim-idx ndelims))))
                (display line)
                (loop #f (+ delim-idx 1)))))
          (display (if zero-term? "\0" "\n"))
          (unless (equal? file "-")
            (close-input-port port))))
      files)))

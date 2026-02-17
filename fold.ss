(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports)

(def (main . args)
  (parameterize ((program-name "fold"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((files .rest)
                (width (string->number (or .?width "80")))
                (bytes? .?bytes)
                (spaces? .?spaces))
            (let (files (if (null? files) '("-") files))
              (for-each
                (lambda (file)
                  (fold-file file width bytes? spaces?))
                files)))))
      args
      program: "fold"
      help: "Wrap input lines in each FILE, writing to standard output."
      (option 'width "-w" "--width"
        help: "use WIDTH columns instead of 80" default: #f)
      (flag 'bytes "-b" "--bytes"
        help: "count bytes rather than columns")
      (flag 'spaces "-s" "--spaces"
        help: "break at spaces")
      (rest-arguments 'rest))))

(def (fold-file file width bytes? spaces?)
  (let (proc
    (lambda (port)
      (let loop ((col 0) (line-buf '()))
        (let (c (read-char port))
          (cond
            ((eof-object? c)
             (for-each write-char (reverse line-buf)))
            ((eqv? c #\newline)
             (for-each write-char (reverse line-buf))
             (newline)
             (loop 0 '()))
            (else
              (let (new-col (if bytes?
                              (+ col 1)
                              (cond
                                ((eqv? c #\tab)
                                 (* (+ (quotient col 8) 1) 8))
                                ((eqv? c #\backspace)
                                 (max 0 (- col 1)))
                                ((eqv? c #\return) 0)
                                (else (+ col 1)))))
                (if (>= new-col width)
                  (if (and spaces? (find-space-break line-buf))
                    ;; Break at last space
                    (let-values (((before after) (split-at-space line-buf)))
                      (for-each write-char (reverse after))
                      (newline)
                      ;; Re-process the part after space + current char
                      (let* ((remaining (reverse before))
                             (rem-width (fold-count remaining bytes?)))
                        (loop (fold-char-width c rem-width bytes?)
                              (cons c (reverse remaining)))))
                    ;; Hard break
                    (begin
                      (for-each write-char (reverse line-buf))
                      (newline)
                      (loop (fold-char-width c 0 bytes?) (list c))))
                  (loop new-col (cons c line-buf))))))))))
    (if (equal? file "-")
      (proc (current-input-port))
      (with-catch
        (lambda (e) (warn "~a: No such file or directory" file))
        (lambda ()
          (let (port (open-input-file file))
            (try (proc port)
              (finally (close-input-port port)))))))))

(def (fold-char-width c col bytes?)
  (if bytes?
    (+ col 1)
    (cond
      ((eqv? c #\tab) (* (+ (quotient col 8) 1) 8))
      ((eqv? c #\backspace) (max 0 (- col 1)))
      ((eqv? c #\return) 0)
      (else (+ col 1)))))

(def (fold-count chars bytes?)
  (foldl (lambda (c acc) (fold-char-width c acc bytes?)) 0 chars))

(def (find-space-break buf)
  (memv #\space buf))

(def (split-at-space buf)
  ;; buf is reversed. Find first space and split there.
  (let loop ((rest buf) (before '()))
    (cond
      ((null? rest) (values buf '()))
      ((eqv? (car rest) #\space)
       (values before (cons #\space (cdr rest))))
      (else
        (loop (cdr rest) (cons (car rest) before))))))

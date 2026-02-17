(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports)

(def (main . args)
  (parameterize ((program-name "wc"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let* ((files .rest)
                 (files (if (null? files) '("-") files))
                 (show-lines? .?lines)
                 (show-words? .?words)
                 (show-bytes? .?bytes)
                 (show-chars? .?chars)
                 (show-max-line? .?max-line-length)
                 ;; Default: show lines, words, bytes
                 (default? (not (or show-lines? show-words? show-bytes?
                                    show-chars? show-max-line?)))
                 (totals (list 0 0 0 0 0))) ;; lines words bytes chars max-line
            (for-each
              (lambda (file)
                (let-values (((lines words bytes chars max-line)
                              (count-file file)))
                  (set! totals (list (+ (list-ref totals 0) lines)
                                     (+ (list-ref totals 1) words)
                                     (+ (list-ref totals 2) bytes)
                                     (+ (list-ref totals 3) chars)
                                     (max (list-ref totals 4) max-line)))
                  (print-counts lines words bytes chars max-line
                                show-lines? show-words? show-bytes?
                                show-chars? show-max-line? default?
                                (if (equal? file "-") "" file))))
              files)
            (when (> (length files) 1)
              (print-counts (list-ref totals 0) (list-ref totals 1)
                            (list-ref totals 2) (list-ref totals 3)
                            (list-ref totals 4)
                            show-lines? show-words? show-bytes?
                            show-chars? show-max-line? default?
                            "total")))))
      args
      program: "wc"
      help: "Print newline, word, and byte counts for each FILE."
      (flag 'lines "-l" "--lines" help: "print the newline counts")
      (flag 'words "-w" "--words" help: "print the word counts")
      (flag 'bytes "-c" "--bytes" help: "print the byte counts")
      (flag 'chars "-m" "--chars" help: "print the character counts")
      (flag 'max-line-length "-L" "--max-line-length"
        help: "print the maximum display width")
      (rest-arguments 'rest))))

(def (count-file file)
  (let (proc
    (lambda (port)
      (let loop ((lines 0) (words 0) (bytes 0) (chars 0)
                 (max-line 0) (line-len 0) (in-word? #f))
        (let (c (read-char port))
          (if (eof-object? c)
            (values lines words bytes chars (max max-line line-len))
            (let* ((b (char->integer c))
                   (byte-count (cond ((< b #x80) 1) ((< b #x800) 2)
                                     ((< b #x10000) 3) (else 4)))
                   (nl? (eqv? c #\newline))
                   (ws? (or nl? (eqv? c #\space) (eqv? c #\tab)
                            (eqv? c #\return) (eqv? c #\page)
                            (eqv? c (integer->char 11)))) ;; vtab
                   (word-start? (and (not in-word?) (not ws?)))
                   (new-words (if word-start? (+ words 1) words)))
              (loop (if nl? (+ lines 1) lines)
                    new-words
                    (+ bytes byte-count)
                    (+ chars 1)
                    (if nl? (max max-line line-len) max-line)
                    (if nl? 0 (+ line-len 1))
                    (not ws?))))))))
    (if (equal? file "-")
      (proc (current-input-port))
      (with-catch
        (lambda (e)
          (warn "~a: No such file or directory" file)
          (values 0 0 0 0 0))
        (lambda ()
          (let (port (open-input-file file))
            (try (proc port)
              (finally (close-input-port port)))))))))

(def (print-counts lines words bytes chars max-line
                    show-lines? show-words? show-bytes?
                    show-chars? show-max-line? default? name)
  (when (or default? show-lines?)
    (display-padded lines 7))
  (when (or default? show-words?)
    (display-padded words 7))
  (when (or default? show-bytes?)
    (display-padded bytes 7))
  (when show-chars?
    (display-padded chars 7))
  (when show-max-line?
    (display-padded max-line 7))
  (when (> (string-length name) 0)
    (display " ")
    (display name))
  (newline))

(def (display-padded n width)
  (let* ((s (number->string n))
         (pad (- width (string-length s))))
    (let loop ((i 0))
      (when (< i pad) (display " ") (loop (+ i 1))))
    (display s)))

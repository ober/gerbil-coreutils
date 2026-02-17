(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports)

(def (main . args)
  (parameterize ((program-name "cat"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((files .rest)
                (number-lines? (or .?number .?number-nonblank))
                (number-nonblank? .?number-nonblank)
                (squeeze? .?squeeze-blank)
                (show-ends? (or .?show-ends .?show-all .?ve))
                (show-tabs? (or .?show-tabs .?show-all .?vt))
                (show-nonprinting? (or .?show-nonprinting .?show-all .?ve .?vt)))
            (let ((line-num 1)
                  (prev-blank? #f))
              (process-cat-files
                (if (null? files) '("-") files)
                (lambda (port)
                  (let loop ()
                    (let (line (read-line port))
                      (unless (eof-object? line)
                        (let* ((blank? (string=? line ""))
                               (skip? (and squeeze? prev-blank? blank?)))
                          (unless skip?
                            (when number-lines?
                              (if (and number-nonblank? blank?)
                                (display "      \t")
                                (begin
                                  (display-right-aligned line-num 6)
                                  (display "\t")
                                  (set! line-num (+ line-num 1)))))
                            (display (transform-line line show-tabs? show-nonprinting?))
                            (when show-ends? (display "$"))
                            (newline))
                          (set! prev-blank? blank?))
                        (loop))))))))))
      args
      program: "cat"
      help: "Concatenate FILE(s) to standard output."
      (flag 'show-all "-A" "--show-all"
        help: "equivalent to -vET")
      (flag 'number-nonblank "-b" "--number-nonblank"
        help: "number nonempty output lines, overrides -n")
      (flag 've "-e"
        help: "equivalent to -vE")
      (flag 'show-ends "-E" "--show-ends"
        help: "display $ at end of each line")
      (flag 'number "-n" "--number"
        help: "number all output lines")
      (flag 'squeeze-blank "-s" "--squeeze-blank"
        help: "suppress repeated empty output lines")
      (flag 'vt "-t"
        help: "equivalent to -vT")
      (flag 'show-tabs "-T" "--show-tabs"
        help: "display TAB characters as ^I")
      (flag 'show-nonprinting "-v" "--show-nonprinting"
        help: "use ^ and M- notation, except for LFD and TAB")
      (rest-arguments 'rest))))

(def (process-cat-files files proc)
  (for-each
    (lambda (f)
      (if (equal? f "-")
        (proc (current-input-port))
        (with-catch
          (lambda (e)
            (warn "~a: No such file or directory" f))
          (lambda ()
            (let (port (open-input-file f))
              (try (proc port)
                (finally (close-input-port port))))))))
    files))

(def (display-right-aligned n width)
  (let* ((s (number->string n))
         (pad (- width (string-length s))))
    (let loop ((i 0))
      (when (< i pad) (display " ") (loop (+ i 1))))
    (display s)))

(def (transform-line line show-tabs? show-nonprinting?)
  (if (and (not show-tabs?) (not show-nonprinting?))
    line
    (let (out (open-output-string))
      (let loop ((i 0))
        (if (>= i (string-length line))
          (get-output-string out)
          (let (c (string-ref line i))
            (cond
              ((and show-tabs? (eqv? c #\tab))
               (display "^I" out))
              ((and show-nonprinting? (< (char->integer c) 32) (not (eqv? c #\tab)))
               (write-char #\^ out)
               (write-char (integer->char (+ (char->integer c) 64)) out))
              ((and show-nonprinting? (= (char->integer c) 127))
               (display "^?" out))
              ((and show-nonprinting? (> (char->integer c) 127))
               (display "M-" out)
               (let (c2 (- (char->integer c) 128))
                 (cond
                   ((< c2 32)
                    (write-char #\^ out)
                    (write-char (integer->char (+ c2 64)) out))
                   ((= c2 127)
                    (display "^?" out))
                   (else
                    (write-char (integer->char c2) out)))))
              (else
                (write-char c out)))
            (loop (+ i 1))))))))

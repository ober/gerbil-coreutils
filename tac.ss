(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports)

(def (main . args)
  (parameterize ((program-name "tac"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((files .rest)
                (separator (or .?separator "\n"))
                (before? .?before))
            (let (files (if (null? files) '("-") files))
              (for-each
                (lambda (file)
                  (tac-file file separator before?))
                files)))))
      args
      program: "tac"
      help: "Write each FILE to standard output, last line first."
      (flag 'before "-b" "--before"
        help: "attach the separator before instead of after")
      (option 'separator "-s" "--separator"
        help: "use STRING as the separator instead of newline"
        default: #f)
      (rest-arguments 'rest))))

(def (tac-file file separator before?)
  (let (proc
    (lambda (port)
      (let* ((content (read-all-as-string port))
             (lines (string-split-sep content separator)))
        ;; Reverse and output
        (let* ((reversed (reverse lines))
               ;; If last element is empty (file ended with separator), skip it
               (reversed (if (and (pair? reversed)
                                   (string=? (car reversed) ""))
                           (cdr reversed)
                           reversed)))
          (let loop ((rest reversed) (first? #t))
            (when (pair? rest)
              (if before?
                (begin
                  (unless first? (void))
                  (display separator)
                  (display (car rest)))
                (begin
                  (display (car rest))
                  (display separator)))
              (loop (cdr rest) #f)))))))
    (if (equal? file "-")
      (proc (current-input-port))
      (with-catch
        (lambda (e) (warn "~a: No such file or directory" file))
        (lambda ()
          (let (port (open-input-file file))
            (try (proc port)
              (finally (close-input-port port)))))))))

(def (string-split-sep str sep)
  (let ((slen (string-length str))
        (seplen (string-length sep)))
    (if (= seplen 0)
      (list str)
      (let loop ((i 0) (start 0) (acc '()))
        (cond
          ((> (+ i seplen) slen)
           (reverse (cons (substring str start slen) acc)))
          ((string=? (substring str i (+ i seplen)) sep)
           (loop (+ i seplen) (+ i seplen)
                 (cons (substring str start i) acc)))
          (else
           (loop (+ i 1) start acc)))))))

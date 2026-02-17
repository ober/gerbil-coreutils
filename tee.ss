(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar)

(def (main . args)
  (parameterize ((program-name "tee"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let* ((files .rest)
                 (append? .?append)
                 (ports (map (lambda (f)
                              (with-catch
                                (lambda (e)
                                  (warn "~a: cannot open for writing" f)
                                  #f)
                                (lambda ()
                                  (open-output-file [path: f
                                                     append: append?]))))
                            files)))
            ;; Copy stdin to stdout and all file ports
            (let loop ()
              (let (c (read-char))
                (unless (eof-object? c)
                  (write-char c)
                  (for-each
                    (lambda (p)
                      (when p
                        (with-catch void (lambda () (write-char c p)))))
                    ports)
                  (loop))))
            ;; Close all ports
            (for-each
              (lambda (p) (when p (close-output-port p)))
              ports))))
      args
      program: "tee"
      help: "Copy standard input to each FILE, and also to standard output."
      (flag 'append "-a" "--append"
        help: "append to the given FILEs, do not overwrite")
      (flag 'ignore-interrupts "-i" "--ignore-interrupts"
        help: "ignore interrupt signals")
      (rest-arguments 'rest))))

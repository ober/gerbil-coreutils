(import :std/test
        :std/misc/process
        :std/misc/ports
        :std/os/temporaries)

(export cat-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (cat-exe) (path-expand "cat" bin-dir))

(def (run-cat-stdin input . args)
  (let* ((proc (open-process [path: (cat-exe)
                               arguments: args
                               stdin-redirection: #t
                               stdout-redirection: #t]))
         (_ (begin (display input proc) (close-output-port proc)))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def (run-cat-file file . args)
  (let* ((proc (open-process [path: (cat-exe)
                               arguments: (append args (list file))
                               stdout-redirection: #t]))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def cat-test
  (test-suite "cat"
    (test-case "passthrough"
      (check (run-cat-stdin "hello\n") => "hello\n"))

    (test-case "-n numbers all lines"
      (check (run-cat-stdin "a\nb\nc\n" "-n")
             => "     1\ta\n     2\tb\n     3\tc\n"))

    (test-case "-b numbers non-blank lines"
      (check (run-cat-stdin "a\n\nb\n" "-b")
             => "     1\ta\n      \t\n     2\tb\n"))

    (test-case "-E shows dollar at end"
      (check (run-cat-stdin "hi\n" "-E") => "hi$\n"))

    (test-case "-T shows tabs as ^I"
      (check (run-cat-stdin "a\tb\n" "-T") => "a^Ib\n"))

    (test-case "-s squeezes blank lines"
      (check (run-cat-stdin "a\n\n\n\nb\n" "-s")
             => "a\n\nb\n"))

    (test-case "file argument"
      (call-with-temporary-file-name "cat-test"
        (lambda (tmp)
          (call-with-output-file tmp (lambda (p) (display "file content\n" p)))
          (check (run-cat-file tmp) => "file content\n"))))))

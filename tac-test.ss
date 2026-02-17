(import :std/test
        :std/misc/process
        :std/misc/ports)

(export tac-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (tac-exe) (path-expand "tac" bin-dir))

(def (run-tac-stdin input . args)
  (let* ((proc (open-process [path: (tac-exe)
                               arguments: args
                               stdin-redirection: #t
                               stdout-redirection: #t]))
         (_ (begin (display input proc) (close-output-port proc)))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def tac-test
  (test-suite "tac"
    (test-case "reverses lines"
      (check (run-tac-stdin "a\nb\nc\n")
             => "c\nb\na\n"))

    (test-case "single line"
      (check (run-tac-stdin "hello\n")
             => "hello\n"))))

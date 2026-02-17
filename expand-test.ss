(import :std/test
        :std/misc/process
        :std/misc/ports)

(export expand-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (expand-exe) (path-expand "expand" bin-dir))
(def (unexpand-exe) (path-expand "unexpand" bin-dir))

(def (run-expand-stdin input . args)
  (let* ((proc (open-process [path: (expand-exe)
                               arguments: args
                               stdin-redirection: #t
                               stdout-redirection: #t]))
         (_ (begin (display input proc) (close-output-port proc)))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def (run-unexpand-stdin input . args)
  (let* ((proc (open-process [path: (unexpand-exe)
                               arguments: args
                               stdin-redirection: #t
                               stdout-redirection: #t]))
         (_ (begin (display input proc) (close-output-port proc)))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def expand-test
  (test-suite "expand and unexpand"
    (test-case "expand replaces tabs with spaces"
      (check (run-expand-stdin "\thello\n")
             => "        hello\n"))

    (test-case "expand -t 4"
      (check (run-expand-stdin "\thello\n" "-t" "4")
             => "    hello\n"))

    (test-case "unexpand converts leading spaces to tabs"
      (check (run-unexpand-stdin "        hello\n")
             => "\thello\n"))

    (test-case "expand then unexpand roundtrip"
      (let* ((expanded (run-expand-stdin "\thello\n"))
             (unexpanded (run-unexpand-stdin expanded)))
        (check unexpanded => "\thello\n")))))

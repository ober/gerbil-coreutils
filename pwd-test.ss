(import :std/test
        :std/misc/process
        :std/misc/ports)

(export pwd-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (pwd-exe) (path-expand "pwd" bin-dir))

(def (run-pwd . args)
  (let* ((proc (open-process [path: (pwd-exe)
                               arguments: args
                               stdout-redirection: #t]))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def pwd-test
  (test-suite "pwd"
    (test-case "outputs a directory"
      (let (out (run-pwd))
        (check (> (string-length out) 1) ? values)
        (check (eqv? (string-ref out 0) #\/) ? values)))

    (test-case "-P physical mode"
      (let (out (run-pwd "-P"))
        (check (> (string-length out) 1) ? values)))))

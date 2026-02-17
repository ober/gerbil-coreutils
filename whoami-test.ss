(import :std/test
        :std/misc/process
        :std/misc/ports)

(export whoami-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (whoami-exe) (path-expand "whoami" bin-dir))

(def (run-whoami . args)
  (let* ((proc (open-process [path: (whoami-exe)
                               arguments: args
                               stdout-redirection: #t]))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def whoami-test
  (test-suite "whoami"
    (test-case "outputs current user"
      (let (out (run-whoami))
        ;; Should match USER env or user-name
        (check (> (string-length out) 1) ? values)
        (check (eqv? (string-ref out (- (string-length out) 1)) #\newline) ? values)))))

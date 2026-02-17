(import :std/test
        :std/misc/process
        :std/misc/ports)

(export printenv-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (printenv-exe) (path-expand "printenv" bin-dir))

(def (run-printenv . args)
  (let* ((proc (open-process [path: (printenv-exe)
                               arguments: args
                               stdout-redirection: #t]))
         (out (read-all-as-string proc))
         (status (process-status proc)))
    (values out status)))

(def printenv-test
  (test-suite "printenv"
    (test-case "prints specific variable"
      (let-values (((out status) (run-printenv "HOME")))
        (check (> (string-length out) 0) ? values)
        (check status => 0)))

    (test-case "missing variable exits non-zero"
      (let-values (((out status) (run-printenv "NONEXISTENT_VAR_12345")))
        (check (> status 0) ? values)))

    (test-case "no args prints all"
      (let-values (((out status) (run-printenv)))
        (check (string-contains out "HOME=") ? values)
        (check status => 0)))))

(def (string-contains haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? (substring haystack i (+ i nlen)) needle) i)
        (else (loop (+ i 1)))))))

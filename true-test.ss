(import :std/test
        :std/misc/process
        :std/misc/ports)

(export true-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (true-exe) (path-expand "true" bin-dir))

(def true-test
  (test-suite "true"
    (test-case "exits with 0"
      (let* ((proc (open-process [path: (true-exe)
                                  stdout-redirection: #f
                                  stderr-redirection: #f]))
             (status (process-status proc)))
        (check status => 0)))

    (test-case "--version outputs version info"
      (let* ((proc (open-process [path: (true-exe)
                                  arguments: '("--version")
                                  stdout-redirection: #t]))
             (out (read-all-as-string proc))
             (status (process-status proc)))
        (check (string-contains out "gerbil-coreutils") ? values)))

    (test-case "--help outputs usage info"
      (let* ((proc (open-process [path: (true-exe)
                                  arguments: '("--help")
                                  stdout-redirection: #t]))
             (out (read-all-as-string proc))
             (status (process-status proc)))
        (check (string-contains out "Exit with") ? values)))))

(def (string-contains haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? (substring haystack i (+ i nlen)) needle) i)
        (else (loop (+ i 1)))))))

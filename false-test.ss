(import :std/test
        :std/misc/process
        :std/misc/ports)

(export false-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (false-exe) (path-expand "false" bin-dir))

(def false-test
  (test-suite "false"
    (test-case "exits with non-zero"
      (let* ((proc (open-process [path: (false-exe)
                                  stdout-redirection: #f
                                  stderr-redirection: #f]))
             (status (process-status proc)))
        (check (> status 0) ? values)))

    (test-case "--version outputs version info"
      (let* ((proc (open-process [path: (false-exe)
                                  arguments: '("--version")
                                  stdout-redirection: #t]))
             (out (read-all-as-string proc))
             (status (process-status proc)))
        (check (string-contains out "gerbil-coreutils") ? values)))))

(def (string-contains haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? (substring haystack i (+ i nlen)) needle) i)
        (else (loop (+ i 1)))))))

(import :std/test
        :std/misc/process
        :std/misc/ports
        :std/os/temporaries)

(export tee-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (tee-exe) (path-expand "tee" bin-dir))

(def tee-test
  (test-suite "tee"
    (test-case "copies stdin to stdout and file"
      (call-with-temporary-file-name "tee-test"
        (lambda (tmp)
          (let* ((proc (open-process [path: (tee-exe)
                                      arguments: (list tmp)
                                      stdin-redirection: #t
                                      stdout-redirection: #t]))
                 (_ (begin (display "hello tee\n" proc)
                           (close-output-port proc)))
                 (out (read-all-as-string proc))
                 (status (process-status proc)))
            (check status => 0)
            (check out => "hello tee\n")
            (check (read-file-string tmp) => "hello tee\n")
            (delete-file tmp)))))

    (test-case "-a appends to file"
      (call-with-temporary-file-name "tee-test"
        (lambda (tmp)
          ;; Write initial content
          (write-file-string tmp "first\n")
          (let* ((proc (open-process [path: (tee-exe)
                                      arguments: (list "-a" tmp)
                                      stdin-redirection: #t
                                      stdout-redirection: #t]))
                 (_ (begin (display "second\n" proc)
                           (close-output-port proc)))
                 (out (read-all-as-string proc))
                 (status (process-status proc)))
            (check status => 0)
            (check (read-file-string tmp) => "first\nsecond\n")
            (delete-file tmp)))))))

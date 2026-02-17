(import :std/test
        :std/misc/process
        :std/misc/ports)

(export sleep-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (sleep-exe) (path-expand "sleep" bin-dir))

(def sleep-test
  (test-suite "sleep"
    (test-case "sleeps for short duration"
      (let* ((start (time->seconds (current-time)))
             (proc (open-process [path: (sleep-exe)
                                  arguments: '("0.1")
                                  stdout-redirection: #f
                                  stderr-redirection: #f]))
             (status (process-status proc))
             (elapsed (- (time->seconds (current-time)) start)))
        (check status => 0)
        (check (>= elapsed 0.05) ? values)))

    (test-case "suffix s"
      (let* ((proc (open-process [path: (sleep-exe)
                                  arguments: '("0.1s")
                                  stdout-redirection: #f
                                  stderr-redirection: #f]))
             (status (process-status proc)))
        (check status => 0)))))

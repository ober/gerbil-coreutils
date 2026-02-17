(import :std/test
        :std/misc/process
        :std/misc/ports
        :std/os/temporaries)

(export link-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (link-exe) (path-expand "link" bin-dir))
(def (unlink-exe) (path-expand "unlink" bin-dir))

(def link-test
  (test-suite "link and unlink"
    (test-case "create and remove hard link"
      (let* ((tmpdir (make-temporary-file-name "link-test"))
             (src (string-append tmpdir "/src"))
             (dst (string-append tmpdir "/dst")))
        (create-directory tmpdir)
        ;; Create source file
        (call-with-output-file src (lambda (p) (display "hello" p)))
        ;; Create hard link
        (let* ((proc (open-process [path: (link-exe)
                                    arguments: (list src dst)
                                    stdout-redirection: #f
                                    stderr-redirection: #f]))
               (status (process-status proc)))
          (check status => 0)
          (check (file-exists? dst) ? values)
          ;; Verify content
          (check (call-with-input-file dst (lambda (p) (read-line p))) => "hello"))
        ;; Unlink
        (let* ((proc (open-process [path: (unlink-exe)
                                    arguments: (list dst)
                                    stdout-redirection: #f
                                    stderr-redirection: #f]))
               (status (process-status proc)))
          (check status => 0)
          (check (not (file-exists? dst)) ? values))
        ;; Cleanup
        (delete-file src)
        (delete-directory tmpdir)))))

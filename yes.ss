(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar)

(def (main . args)
  (parameterize ((program-name "yes"))
    (when (and (pair? args) (member (car args) '("--help" "--version")))
      (cond
        ((equal? (car args) "--help")
         (displayln "Usage: yes [STRING]...")
         (displayln "Repeatedly output a line with all specified STRING(s), or 'y'.")
         (exit 0))
        ((equal? (car args) "--version")
         (version-info "yes")
         (exit 0))))
    (let* ((line (if (null? args) "y" (string-join args " ")))
           ;; Build a buffer of repeated lines for performance
           (buf (let (out (open-output-string))
                  (let loop ((i 0))
                    (when (< i 512)
                      (display line out)
                      (newline out)
                      (loop (+ i 1))))
                  (get-output-string out))))
      (with-catch
        (lambda (e) (void)) ;; exit silently on broken pipe
        (lambda ()
          (let loop ()
            (display buf)
            (force-output)
            (loop)))))))

;; Helper to join strings
(def (string-join strs sep)
  (if (null? strs) ""
    (let loop ((rest (cdr strs)) (acc (car strs)))
      (if (null? rest) acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))

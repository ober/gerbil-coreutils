(import :std/test
        :std/misc/process
        :std/misc/ports)

(export fold-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (fold-exe) (path-expand "fold" bin-dir))

(def (run-fold-stdin input . args)
  (let* ((proc (open-process [path: (fold-exe)
                               arguments: args
                               stdin-redirection: #t
                               stdout-redirection: #t]))
         (_ (begin (display input proc) (close-output-port proc)))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def fold-test
  (test-suite "fold"
    (test-case "wraps at 10 columns"
      (check (run-fold-stdin "1234567890abcde\n" "-w" "10")
             => "1234567890\nabcde\n"))

    (test-case "short lines unchanged"
      (check (run-fold-stdin "hello\n" "-w" "80")
             => "hello\n"))

    (test-case "-s break at spaces"
      (check (run-fold-stdin "hello world foo\n" "-w" "10" "-s")
             => "hello \nworld foo\n"))))

(import :std/test
        :std/misc/process
        :std/misc/ports)

(export head-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (head-exe) (path-expand "head" bin-dir))

(def (run-head-stdin input . args)
  (let* ((proc (open-process [path: (head-exe)
                               arguments: args
                               stdin-redirection: #t
                               stdout-redirection: #t]))
         (_ (begin (display input proc) (close-output-port proc)))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def head-test
  (test-suite "head"
    (test-case "default 10 lines"
      (let (input (apply string-append
                    (map (lambda (i) (string-append "line" (number->string i) "\n"))
                         '(1 2 3 4 5 6 7 8 9 10 11 12))))
        (check (run-head-stdin input)
               => (apply string-append
                    (map (lambda (i) (string-append "line" (number->string i) "\n"))
                         '(1 2 3 4 5 6 7 8 9 10))))))

    (test-case "-n 3"
      (check (run-head-stdin "a\nb\nc\nd\ne\n" "-n" "3")
             => "a\nb\nc\n"))

    (test-case "-c 5 bytes"
      (check (run-head-stdin "hello world\n" "-c" "5")
             => "hello"))

    (test-case "fewer lines than requested"
      (check (run-head-stdin "a\nb\n" "-n" "5")
             => "a\nb\n"))))

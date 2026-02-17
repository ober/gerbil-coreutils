(import :std/test
        :std/misc/process
        :std/misc/ports)

(export tail-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (tail-exe) (path-expand "tail" bin-dir))

(def (run-tail-stdin input . args)
  (let* ((proc (open-process [path: (tail-exe)
                               arguments: args
                               stdin-redirection: #t
                               stdout-redirection: #t]))
         (_ (begin (display input proc) (close-output-port proc)))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def tail-test
  (test-suite "tail"
    (test-case "-n 2 last two lines"
      (check (run-tail-stdin "a\nb\nc\nd\ne\n" "-n" "2")
             => "d\ne\n"))

    (test-case "-n +3 from line 3"
      (check (run-tail-stdin "a\nb\nc\nd\ne\n" "-n" "+3")
             => "c\nd\ne\n"))

    (test-case "-c 5 last 5 bytes"
      (check (run-tail-stdin "hello world\n" "-c" "5")
             => "orld\n"))

    (test-case "fewer lines than requested"
      (check (run-tail-stdin "a\nb\n" "-n" "5")
             => "a\nb\n"))))

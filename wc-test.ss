(import :std/test
        :std/misc/process
        :std/misc/ports)

(export wc-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (wc-exe) (path-expand "wc" bin-dir))

(def (run-wc-stdin input . args)
  (let* ((proc (open-process [path: (wc-exe)
                               arguments: args
                               stdin-redirection: #t
                               stdout-redirection: #t]))
         (_ (begin (display input proc) (close-output-port proc)))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def wc-test
  (test-suite "wc"
    (test-case "counts lines, words, bytes"
      (check (run-wc-stdin "hello world\n")
             => "      1      2     12\n"))

    (test-case "-l lines only"
      (check (run-wc-stdin "a\nb\nc\n" "-l")
             => "      3\n"))

    (test-case "-w words only"
      (check (run-wc-stdin "hello world foo\n" "-w")
             => "      3\n"))

    (test-case "-c bytes only"
      (check (run-wc-stdin "hello\n" "-c")
             => "      6\n"))

    (test-case "empty input"
      (check (run-wc-stdin "")
             => "      0      0      0\n"))))

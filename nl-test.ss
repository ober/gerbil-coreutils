(import :std/test
        :std/misc/process
        :std/misc/ports)

(export nl-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (nl-exe) (path-expand "nl" bin-dir))

(def (run-nl-stdin input . args)
  (let* ((proc (open-process [path: (nl-exe)
                               arguments: args
                               stdin-redirection: #t
                               stdout-redirection: #t]))
         (_ (begin (display input proc) (close-output-port proc)))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def nl-test
  (test-suite "nl"
    (test-case "default numbering (non-blank)"
      (check (run-nl-stdin "a\n\nb\n")
             => "     1\ta\n      \t\n     2\tb\n"))

    (test-case "-b a number all"
      (check (run-nl-stdin "a\n\nb\n" "-b" "a")
             => "     1\ta\n     2\t\n     3\tb\n"))

    (test-case "-b n number none"
      (check (run-nl-stdin "a\nb\n" "-b" "n")
             => "      \ta\n      \tb\n"))))

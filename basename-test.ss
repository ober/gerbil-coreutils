(import :std/test
        :std/misc/process
        :std/misc/ports)

(export basename-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (basename-exe) (path-expand "basename" bin-dir))

(def (run-basename . args)
  (let* ((proc (open-process [path: (basename-exe)
                               arguments: args
                               stdout-redirection: #t]))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def basename-test
  (test-suite "basename"
    (test-case "simple path"
      (check (run-basename "/usr/bin/sort") => "sort\n"))

    (test-case "strip suffix"
      (check (run-basename "include/stdio.h" ".h") => "stdio\n"))

    (test-case "trailing slash"
      (check (run-basename "/usr/bin/") => "bin\n"))

    (test-case "just filename"
      (check (run-basename "hello.txt") => "hello.txt\n"))

    (test-case "root"
      (check (run-basename "/") => "/\n"))

    (test-case "-s suffix option"
      (check (run-basename "-s" ".ss" "hello.ss") => "hello\n"))

    (test-case "-a multiple"
      (check (run-basename "-a" "/usr/bin/a" "/usr/bin/b")
             => "a\nb\n"))))

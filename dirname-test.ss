(import :std/test
        :std/misc/process
        :std/misc/ports)

(export dirname-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (dirname-exe) (path-expand "dirname" bin-dir))

(def (run-dirname . args)
  (let* ((proc (open-process [path: (dirname-exe)
                               arguments: args
                               stdout-redirection: #t]))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def dirname-test
  (test-suite "dirname"
    (test-case "simple path"
      (check (run-dirname "/usr/bin/sort") => "/usr/bin\n"))

    (test-case "trailing slash"
      (check (run-dirname "/usr/bin/") => "/usr\n"))

    (test-case "no slash"
      (check (run-dirname "hello.txt") => ".\n"))

    (test-case "root"
      (check (run-dirname "/") => "/\n"))

    (test-case "multiple arguments"
      (check (run-dirname "/a/b" "/c/d") => "/a\n/c\n"))))

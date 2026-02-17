(import :std/test
        :std/misc/process
        :std/misc/ports)

(export echo-test)

(def bin-dir
  (path-expand ".gerbil/bin" (current-directory)))

(def (echo-exe) (path-expand "echo" bin-dir))

(def (run-echo . args)
  (let* ((proc (open-process [path: (echo-exe)
                               arguments: args
                               stdout-redirection: #t]))
         (out (read-all-as-string proc)))
    (process-status proc)
    out))

(def echo-test
  (test-suite "echo"
    (test-case "basic output with newline"
      (check (run-echo "hello" "world") => "hello world\n"))

    (test-case "no arguments prints just newline"
      (check (run-echo) => "\n"))

    (test-case "-n suppresses trailing newline"
      (check (run-echo "-n" "hello") => "hello"))

    (test-case "-e enables escape sequences"
      (check (run-echo "-e" "hello\\nworld") => "hello\nworld\n"))

    (test-case "-e tab escape"
      (check (run-echo "-e" "a\\tb") => "a\tb\n"))

    (test-case "-E disables escape sequences"
      (check (run-echo "-E" "hello\\nworld") => "hello\\nworld\n"))

    (test-case "-ne combined flags"
      (check (run-echo "-ne" "hello\\n") => "hello\n"))

    (test-case "non-flag dash arg is literal"
      (check (run-echo "-x" "hello") => "-x hello\n"))

    (test-case "-e backslash escape"
      (check (run-echo "-e" "a\\\\b") => "a\\b\n"))

    (test-case "-e octal escape"
      (check (run-echo "-e" "\\0101") => "A\n"))))

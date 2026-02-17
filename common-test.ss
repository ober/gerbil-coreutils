(import :std/test
        :gerbil-coreutils/common
        :gerbil-coreutils/common/io
        :gerbil-coreutils/common/version)

(export common-test)

(def common-test
  (test-suite "common modules"
    (test-case "EXIT_SUCCESS and EXIT_FAILURE"
      (check EXIT_SUCCESS => 0)
      (check EXIT_FAILURE => 1))

    (test-case "program-name parameter"
      (check (program-name) => "coreutils")
      (parameterize ((program-name "test-prog"))
        (check (program-name) => "test-prog")))

    (test-case "warn outputs to stderr"
      (check (parameterize ((program-name "test"))
               (with-output-to-string
                 (lambda ()
                   (parameterize ((current-error-port (current-output-port)))
                     (warn "hello ~a" "world")))))
             => "test: hello world\n"))

    (test-case "version-info"
      (check (with-output-to-string
               (lambda () (version-info "test")))
             => (string-append "test (gerbil-coreutils) " coreutils-version "\n")))

    (test-case "read-line/delim with newline"
      (let (port (open-input-string "hello\nworld\n"))
        (check (read-line/delim port #\newline) => "hello")
        (check (read-line/delim port #\newline) => "world")))

    (test-case "read-line/delim with NUL"
      (let (port (open-input-string "hello\0world\0"))
        (check (read-line/delim port #\nul) => "hello")
        (check (read-line/delim port #\nul) => "world")))

    (test-case "read-line/delim returns eof"
      (let (port (open-input-string ""))
        (check (eof-object? (read-line/delim port #\nul)) ? values)))

    (test-case "for-each-line"
      (let* ((port (open-input-string "a\nb\nc\n"))
             (result '()))
        (for-each-line port (lambda (line) (set! result (cons line result))))
        (check (reverse result) => '("a" "b" "c"))))

    (test-case "write-line/delim"
      (check (with-output-to-string
               (lambda () (write-line/delim "hello" #\newline)))
             => "hello\n"))))

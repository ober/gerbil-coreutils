(export
  program-name
  die
  warn
  try-help
  EXIT_SUCCESS
  EXIT_FAILURE)

(import :std/format)

;; Exit codes
(def EXIT_SUCCESS 0)
(def EXIT_FAILURE 1)

;; Current program name - set from argv[0]
(def program-name (make-parameter "coreutils"))

;; Print error message to stderr and exit with code
(def (die fmt . args)
  (eprintf "~a: ~?\n" (program-name) fmt args)
  (exit EXIT_FAILURE))

;; Print warning message to stderr
(def (warn fmt . args)
  (eprintf "~a: ~?\n" (program-name) fmt args))

;; Print "Try 'prog --help' for more information."
(def (try-help)
  (eprintf "Try '~a --help' for more information.\n" (program-name))
  (exit EXIT_FAILURE))

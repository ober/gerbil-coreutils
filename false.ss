(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt)

(def (main . args)
  (parameterize ((program-name "false"))
    (when (and (pair? args) (member (car args) '("--help" "--version")))
      (cond
        ((equal? (car args) "--help")
         (displayln "Usage: false")
         (displayln "Exit with a status code indicating failure."))
        ((equal? (car args) "--version")
         (version-info "false"))))
    (exit 1)))

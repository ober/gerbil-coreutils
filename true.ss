(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt)

(def (main . args)
  (parameterize ((program-name "true"))
    (when (and (pair? args) (member (car args) '("--help" "--version")))
      (cond
        ((equal? (car args) "--help")
         (displayln "Usage: true")
         (displayln "Exit with a status code indicating success."))
        ((equal? (car args) "--version")
         (version-info "true"))))
    (exit 0)))

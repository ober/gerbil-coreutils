(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt)

(def (main . args)
  (parameterize ((program-name "whoami"))
    (call-with-getopt
      (lambda (opt)
        (displayln (user-name)))
      args
      program: "whoami"
      help: "Print the user name associated with the current effective user ID.")))

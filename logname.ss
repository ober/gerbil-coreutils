(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt)

(def (main . args)
  (parameterize ((program-name "logname"))
    (call-with-getopt
      (lambda (opt)
        (let (name (getenv "LOGNAME" #f))
          (if name
            (displayln name)
            (die "no login name"))))
      args
      program: "logname"
      help: "Print the name of the current user.")))

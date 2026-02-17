(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar)

(def (main . args)
  (parameterize ((program-name "pwd"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (if .?logical
            ;; Logical: use $PWD if valid
            (let (pwd (getenv "PWD" #f))
              (if (and pwd (file-exists? pwd))
                (displayln pwd)
                (displayln (current-directory))))
            ;; Physical (default): resolve symlinks
            (displayln (current-directory)))))
      args
      program: "pwd"
      help: "Print the full filename of the current working directory."
      (flag 'logical "-L" "--logical"
        help: "use PWD from environment, even if it contains symlinks")
      (flag 'physical "-P" "--physical"
        help: "avoid all symlinks (default)"))))

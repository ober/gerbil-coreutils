(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar)

(def (main . args)
  (parameterize ((program-name "unlink"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let (files .rest)
            (cond
              ((null? files)
               (die "missing operand"))
              ((> (length files) 1)
               (die "extra operand '~a'" (cadr files)))
              (else
                (with-catch
                  (lambda (e)
                    (die "cannot unlink '~a': No such file or directory" (car files)))
                  (lambda ()
                    (delete-file (car files)))))))))
      args
      program: "unlink"
      help: "Call the unlink function to remove the specified FILE."
      (rest-arguments 'rest))))

(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar)

(def (main . args)
  (parameterize ((program-name "link"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let (files .rest)
            (cond
              ((< (length files) 2)
               (die "missing operand"))
              ((> (length files) 2)
               (die "extra operand '~a'" (caddr files)))
              (else
                (with-catch
                  (lambda (e)
                    (die "cannot create link '~a' to '~a': ~a"
                         (cadr files) (car files)
                         (if (os-exception? e)
                           (err-code->string (os-exception-code e))
                           "unknown error")))
                  (lambda ()
                    (create-link (car files) (cadr files)))))))))
      args
      program: "link"
      help: "Call the link function to create a link named FILE2 to an existing FILE1."
      (rest-arguments 'rest))))

(def (err-code->string code)
  (cond
    ((eqv? code 2) "No such file or directory")
    ((eqv? code 13) "Permission denied")
    ((eqv? code 17) "File exists")
    (else (string-append "error code " (number->string code)))))

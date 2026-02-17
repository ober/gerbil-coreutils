(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar)

(def (main . args)
  (parameterize ((program-name "sleep"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (when (null? .rest)
            (die "missing operand"))
          (let (total (foldl + 0 (map parse-duration .rest)))
            (thread-sleep! total))))
      args
      program: "sleep"
      help: "Pause for NUMBER seconds. SUFFIX may be 's' (seconds, default), 'm' (minutes), 'h' (hours), or 'd' (days)."
      (rest-arguments 'rest help: "NUMBER[SUFFIX]..."))))

;; Parse a duration string like "1.5s", "2m", "3h", "1d"
(def (parse-duration str)
  (let* ((len (string-length str))
         (last-char (if (> len 0) (string-ref str (- len 1)) #\nul)))
    (let-values (((num-str mult)
                  (case last-char
                    ((#\s) (values (substring str 0 (- len 1)) 1))
                    ((#\m) (values (substring str 0 (- len 1)) 60))
                    ((#\h) (values (substring str 0 (- len 1)) 3600))
                    ((#\d) (values (substring str 0 (- len 1)) 86400))
                    (else  (values str 1)))))
      (let (num (string->number num-str))
        (unless num
          (die "invalid time interval '~a'" str))
        (when (< num 0)
          (die "invalid time interval '~a'" str))
        (* num mult)))))

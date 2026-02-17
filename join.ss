(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports
        :std/srfi/13)

(def (main . args)
  (parameterize ((program-name "join"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((files .rest)
                (field1 (if .?field1 (string->number .?field1) 1))
                (field2 (if .?field2 (string->number .?field2) 1))
                (sep (if .?separator (string-ref .?separator 0) #f))
                (empty (or .?empty ""))
                (print-unpaired .?print-unpaired)
                (ignore-case? .?ignore-case)
                (header? .?header)
                (zero-term? .?zero-terminated))
            (when (not (= (length files) 2))
              (die "requires exactly two file arguments"))
            (join-files (car files) (cadr files)
                        field1 field2 sep empty
                        print-unpaired ignore-case?
                        header? zero-term?))))
      args
      program: "join"
      help: "Join lines of two files on a common field."
      (option 'field1 "-1" "-1"
        help: "join on this FIELD of file 1" default: #f)
      (option 'field2 "-2" "-2"
        help: "join on this FIELD of file 2" default: #f)
      (option 'separator "-t" "-t"
        help: "use CHAR as input and output field separator" default: #f)
      (option 'empty "-e" "-e"
        help: "replace missing input fields with EMPTY" default: #f)
      (option 'print-unpaired "-a" "-a"
        help: "also print unpairable lines from file FILENUM (1 or 2)" default: #f)
      (flag 'ignore-case "-i" "--ignore-case"
        help: "ignore differences in case when comparing fields")
      (flag 'header "--header" "--header"
        help: "treat the first line in each file as field headers")
      (flag 'zero-terminated "-z" "--zero-terminated"
        help: "line delimiter is NUL, not newline")
      (rest-arguments 'rest))))

(def (open-join-port file)
  (if (equal? file "-")
    (current-input-port)
    (open-input-file file)))

(def (split-fields line sep)
  (if sep
    (string-split line sep)
    ;; Default: split on runs of whitespace
    (let ((tokens (string-tokenize line)))
      (if (null? tokens) '("") tokens))))

(def (get-field fields n)
  (if (and (>= n 1) (<= n (length fields)))
    (list-ref fields (- n 1))
    ""))

(def (join-files file1 file2 field1 field2 sep empty
                  print-unpaired ignore-case? header? zero-term?)
  (let ((port1 (open-join-port file1))
        (port2 (open-join-port file2))
        (delim (if zero-term? #\nul #\newline))
        (eol (if zero-term? "\0" "\n"))
        (out-sep (if sep (string sep) " "))
        (unpaired-1? (and print-unpaired (member print-unpaired '("1" "0"))))
        (unpaired-2? (and print-unpaired (member print-unpaired '("2" "0")))))

    ;; Handle header line
    (when header?
      (let ((h1 (read-line port1 delim))
            (h2 (read-line port2 delim)))
        (unless (or (eof-object? h1) (eof-object? h2))
          (let ((f1 (split-fields h1 sep))
                (f2 (split-fields h2 sep)))
            (output-joined-line (get-field f1 field1) f1 f2 field1 field2 out-sep empty)
            (display eol)))))

    (let loop ((l1 (read-line port1 delim))
               (l2 (read-line port2 delim)))
      (cond
        ((and (eof-object? l1) (eof-object? l2))
         'done)
        ((eof-object? l1)
         (when unpaired-2?
           (display l2) (display eol))
         (loop l1 (read-line port2 delim)))
        ((eof-object? l2)
         (when unpaired-1?
           (display l1) (display eol))
         (loop (read-line port1 delim) l2))
        (else
         (let* ((f1 (split-fields l1 sep))
                (f2 (split-fields l2 sep))
                (k1 (get-field f1 field1))
                (k2 (get-field f2 field2))
                (cmp (if ignore-case?
                       (string-ci<=> k1 k2)
                       (string<=> k1 k2))))
           (cond
             ((< cmp 0)
              (when unpaired-1?
                (display l1) (display eol))
              (loop (read-line port1 delim) l2))
             ((> cmp 0)
              (when unpaired-2?
                (display l2) (display eol))
              (loop l1 (read-line port2 delim)))
             (else
              ;; Match - output joined line
              (output-joined-line k1 f1 f2 field1 field2 out-sep empty)
              (display eol)
              (loop (read-line port1 delim)
                    (read-line port2 delim))))))))

    (unless (equal? file1 "-") (close-input-port port1))
    (unless (equal? file2 "-") (close-input-port port2))))

(def (string<=> a b)
  (cond ((string<? a b) -1)
        ((string>? a b) 1)
        (else 0)))

(def (string-ci<=> a b)
  (cond ((string-ci<? a b) -1)
        ((string-ci>? a b) 1)
        (else 0)))

(def (output-joined-line key f1 f2 field1 field2 sep empty)
  (display key)
  ;; Output fields from file1 (except join field)
  (let loop ((i 1) (fields f1))
    (when (pair? fields)
      (unless (= i field1)
        (display sep)
        (let ((v (car fields)))
          (display (if (string-null? v) empty v))))
      (loop (+ i 1) (cdr fields))))
  ;; Output fields from file2 (except join field)
  (let loop ((i 1) (fields f2))
    (when (pair? fields)
      (unless (= i field2)
        (display sep)
        (let ((v (car fields)))
          (display (if (string-null? v) empty v))))
      (loop (+ i 1) (cdr fields)))))

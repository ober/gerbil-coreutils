(export main)

(import :gerbil-coreutils/common
        :gerbil-coreutils/common/version
        :std/cli/getopt
        :std/sugar
        :std/misc/ports
        :std/format
        :std/srfi/13)

(def (main . args)
  (parameterize ((program-name "numfmt"))
    (call-with-getopt
      (lambda (opt)
        (let-hash opt
          (let ((from-mode (or .?from "none"))
                (to-mode (or .?to "none"))
                (from-unit (if .?from-unit (string->number .?from-unit) 1))
                (to-unit (if .?to-unit (string->number .?to-unit) 1))
                (padding (if .?padding (string->number .?padding) 0))
                (round-mode (or .?round "from-zero"))
                (suffix (or .?suffix ""))
                (header-lines (if .?header (string->number .?header) 0))
                (zero-term? .?zero-terminated)
                (rest-nums .rest))
            (if (null? rest-nums)
              ;; Read from stdin
              (let ((delim (if zero-term? #\nul #\newline)))
                (let loop ((hdr header-lines))
                  (let (line (read-line (current-input-port) delim))
                    (unless (eof-object? line)
                      (if (> hdr 0)
                        (begin (display line) (newline)
                               (loop (- hdr 1)))
                        (begin
                          (process-numfmt-line line from-mode to-mode
                                              from-unit to-unit padding
                                              round-mode suffix)
                          (loop 0)))))))
              ;; Process arguments
              (for-each
                (lambda (num-str)
                  (process-numfmt-line num-str from-mode to-mode
                                      from-unit to-unit padding
                                      round-mode suffix))
                rest-nums)))))
      args
      program: "numfmt"
      help: "Reformat numbers."
      (option 'from "--from" "--from"
        help: "auto-scale input numbers (auto, si, iec, iec-i)" default: #f)
      (option 'to "--to" "--to"
        help: "auto-scale output numbers (auto, si, iec, iec-i)" default: #f)
      (option 'from-unit "--from-unit" "--from-unit"
        help: "specify the input unit size" default: #f)
      (option 'to-unit "--to-unit" "--to-unit"
        help: "specify the output unit size" default: #f)
      (option 'padding "--padding" "--padding"
        help: "pad the output to N characters" default: #f)
      (option 'round "--round" "--round"
        help: "rounding method: up, down, from-zero, towards-zero, nearest"
        default: #f)
      (option 'suffix "--suffix" "--suffix"
        help: "add SUFFIX to output numbers" default: #f)
      (option 'header "--header" "--header"
        help: "print without conversion the first N header lines" default: #f)
      (flag 'zero-terminated "-z" "--zero-terminated"
        help: "line delimiter is NUL, not newline")
      (rest-arguments 'rest))))

(def (process-numfmt-line line from-mode to-mode from-unit to-unit
                           padding round-mode suffix)
  (let* ((trimmed (string-trim line))
         (val (parse-numfmt-input trimmed from-mode from-unit)))
    (if val
      (let* ((scaled (/ val to-unit))
             (result (format-numfmt-output scaled to-mode round-mode suffix)))
        (if (and padding (not (= padding 0)))
          (let ((padded (pad-string result padding)))
            (display padded))
          (display result))
        (newline))
      (begin
        (warn "invalid number: '~a'" trimmed)
        (display line)
        (newline)))))

(def (parse-numfmt-input str mode unit)
  (if (equal? mode "none")
    (let ((n (string->number str)))
      (if n (* n unit) #f))
    ;; Parse with suffix
    (let* ((len (string-length str)))
      (if (= len 0) #f
        (let* ((last-c (string-ref str (- len 1)))
               (suffix-info (parse-suffix last-c mode))
               (num-str (if (car suffix-info)
                          (substring str 0 (- len (if (and (> len 1)
                                                          (eqv? (string-ref str (- len 1)) #\i))
                                                    2 1)))
                          str))
               (mult (cdr suffix-info)))
          ;; Handle iec-i suffix (e.g., Ki, Mi)
          (let ((num-str (if (and (string-suffix? "i" str)
                                  (member mode '("iec-i" "auto")))
                           (substring str 0 (- len 1))
                           num-str)))
            (let ((n (string->number num-str)))
              (if n (* n mult unit) #f))))))))

(def (parse-suffix c mode)
  (let ((base (if (member mode '("si" "auto")) 1000 1024)))
    (case (char-upcase c)
      ((#\K) (cons #t base))
      ((#\M) (cons #t (* base base)))
      ((#\G) (cons #t (* base base base)))
      ((#\T) (cons #t (* base base base base)))
      ((#\P) (cons #t (* base base base base base)))
      ((#\E) (cons #t (* base base base base base base)))
      (else (cons #f 1)))))

(def (format-numfmt-output val mode round-mode suffix)
  (if (equal? mode "none")
    (string-append (format-number val round-mode) suffix)
    ;; Format with suffix
    (let ((base (if (member mode '("si" "auto")) 1000 1024))
          (suffixes '("" "K" "M" "G" "T" "P" "E"))
          (iec-i? (equal? mode "iec-i")))
      (let loop ((v (abs val)) (idx 0) (rest suffixes))
        (if (or (null? (cdr rest)) (< v base))
          (let* ((sign (if (< val 0) "-" ""))
                 (num (round-number v round-mode))
                 (suf (car rest))
                 (suf (if (and iec-i? (not (string-null? suf)))
                        (string-append suf "i") suf)))
            (string-append sign (format-rounded num) suf suffix))
          (loop (/ v base) (+ idx 1) (cdr rest)))))))

(def (format-number val round-mode)
  (let ((rounded (round-number (abs val) round-mode)))
    (string-append (if (< val 0) "-" "")
                   (format-rounded rounded))))

(def (format-rounded val)
  (if (integer? val)
    (number->string (inexact->exact val))
    (let ((s (number->string (inexact val))))
      ;; Trim trailing zeros after decimal
      (if (string-contains s ".")
        (let loop ((i (- (string-length s) 1)))
          (cond
            ((eqv? (string-ref s i) #\0) (loop (- i 1)))
            ((eqv? (string-ref s i) #\.) (substring s 0 i))
            (else (substring s 0 (+ i 1)))))
        s))))

(def (round-number val mode)
  (cond
    ((equal? mode "up") (ceiling val))
    ((equal? mode "down") (floor val))
    ((equal? mode "towards-zero") (truncate val))
    ((equal? mode "from-zero")
     (if (>= val 0) (ceiling val) (floor val)))
    ((equal? mode "nearest") (round val))
    (else (round val))))

(def (pad-string str padding)
  (let ((width (abs padding))
        (len (string-length str)))
    (if (>= len width) str
      (let ((pad (make-string (- width len) #\space)))
        (if (> padding 0)
          (string-append pad str)  ;; right-align
          (string-append str pad))))))  ;; left-align

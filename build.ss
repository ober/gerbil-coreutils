#!/usr/bin/env gxi
(import :std/build-script)

(defbuild-script
  '("common"
    "common/io"
    "common/version"
    ;; Phase 1
    (exe: "true" bin: "true")
    (exe: "false" bin: "false")
    (exe: "yes" bin: "yes")
    (exe: "echo" bin: "echo")
    (exe: "printenv" bin: "printenv")
    (exe: "sleep" bin: "sleep")
    (exe: "whoami" bin: "whoami")
    (exe: "logname" bin: "logname")
    (exe: "pwd" bin: "pwd")
    (exe: "basename" bin: "basename")
    (exe: "dirname" bin: "dirname")
    (exe: "link" bin: "link")
    (exe: "unlink" bin: "unlink")
    ;; Phase 2
    (exe: "cat" bin: "cat")
    (exe: "head" bin: "head")
    (exe: "tail" bin: "tail")
    (exe: "wc" bin: "wc")
    (exe: "tee" bin: "tee")
    (exe: "tac" bin: "tac")
    (exe: "nl" bin: "nl")
    (exe: "fold" bin: "fold")
    (exe: "expand" bin: "expand")
    (exe: "unexpand" bin: "unexpand")
    ;; Phase 3
    (exe: "cut" bin: "cut")
    (exe: "paste" bin: "paste")
    (exe: "join" bin: "join")
    (exe: "comm" bin: "comm")
    (exe: "sort" bin: "sort")
    (exe: "uniq" bin: "uniq")
    (exe: "tr" bin: "tr")
    (exe: "numfmt" bin: "numfmt")))

#!/usr/bin/env gxi
(import :std/build-script)

(defbuild-script
  '("common"
    "common/io"
    "common/version"
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
    (exe: "unlink" bin: "unlink")))

# Gerbil Coreutils — Implementation Plan

An exact behavioral replica of GNU coreutils, implemented in Gerbil Scheme.

## Project Architecture

### Directory Structure

```
gerbil-coreutils/
  gerbil.pkg                    # Package definition
  build.ss                      # Build script (all exe targets)
  Makefile                      # Build/test/install automation
  README.md                     # Project documentation

  # Shared infrastructure
  common.ss                     # Error reporting, exit codes, program name
  common/
    io.ss                       # Line-by-line processing, stdin/file dispatch
    getopt.ss                   # CLI option parsing helpers (wraps :std/cli/getopt)
    numeric.ss                  # Number parsing with K/M/G suffix multipliers
    permissions.ss              # chmod-style symbolic/octal permission parsing
    fileutils.ss                # File operations (copy, remove, stat wrappers)
    timeutils.ss                # Timestamp parsing/formatting
    version.ss                  # --version / --help boilerplate

  # Individual utility modules (each exports `main`)
  cat.ss
  echo.ss
  true.ss
  false.ss
  yes.ss
  ... (one per utility)

  # Tests
  cat-test.ss
  echo-test.ss
  ... (one per utility)
```

### Design Principles

1. **Each utility is a standalone module** with `(export main)`. Can be built as
   individual exe or linked into a multicall binary.
2. **Shared `common/` modules** prevent code duplication across utilities.
3. **Behavioral fidelity over performance**: Match GNU coreutils output byte-for-byte.
   Optimize only where Gerbil idioms are naturally efficient.
4. **Use Gerbil's `:std/cli/getopt`** for option parsing — provides `--help` and
   `--version` handling automatically.
5. **POSIX compliance**: Respect `POSIXLY_CORRECT` environment variable where GNU
   coreutils does.
6. **Test-driven**: Every utility gets a test suite verifying behavior against
   GNU coreutils output.

### Key Gerbil Libraries Used

| Library               | Purpose                                                 |
|-----------------------|---------------------------------------------------------|
| `:std/cli/getopt`     | Command-line option parsing                             |
| `:std/misc/ports`     | File I/O (read-file-string, read-file-lines, copy-port) |
| `:std/misc/path`      | Path manipulation utilities                             |
| `:std/pregexp`        | Regular expressions (for nl, expr, etc.)                |
| `:std/format`         | Printf-style formatting                                 |
| `:std/srfi/13`        | String utilities (string-join, etc.)                    |
| `:std/sort`           | Sorting                                                 |
| `:std/os/signal`      | Signal handling (kill, timeout)                         |
| `:std/os/fcntl`       | File descriptor operations                              |
| `:std/os/fd`          | FD open/close                                           |
| `:std/os/temporaries` | Temp file creation                                      |
| `:std/os/hostname`    | hostname                                                |
| `:std/os/pid`         | getpid/getppid                                          |
| `:std/crypto`         | MD5/SHA hash algorithms                                 |
| Gambit builtins       | file-info, directory-files, user-info, getenv, etc.     |

---

## Phases

All phases follow a consistent pattern:
1. Look up cookbook recipes (`gerbil_howto`)
2. Check module exports (`gerbil_module_exports`)
3. Write code with syntax/compile checks
4. Build and run tests
5. Verify against GNU coreutils output

---

## Phase 0: Project Scaffolding & Shared Infrastructure

**Goal**: Set up the project structure and shared modules that all utilities depend on.

### 0.1 — Project files
- `gerbil.pkg` with package name `gerbil-coreutils`
- `build.ss` — initially empty exe list, will grow per phase
- `Makefile` with `build`, `test`, `clean`, `install` targets

### 0.2 — `common.ss` — Core shared utilities
- `program-name` parameter — set from `argv[0]`
- `die fmt args...` — print error to stderr and exit 1
- `warn fmt args...` — print warning to stderr
- `try-help` — print "Try 'prog --help' for more information."
- Exit code constants: `EXIT_SUCCESS`, `EXIT_FAILURE`

### 0.3 — `common/io.ss` — I/O processing patterns
- `process-files files proc` — iterate over files (or stdin if empty);
  treat `"-"` as stdin
- `for-each-line port proc` — call proc on each line from port
- `read-line/delim port delim` — read until delimiter (for `-z` NUL support)
- `write-line/delim line delim` — write with delimiter
- `safe-open-input-file path` — open with error handling

### 0.4 — `common/getopt.ss` — CLI helpers
- Wrapper macros for common patterns: `with-parsed-options`
- Standard `--help` / `--version` behavior

### 0.5 — `common/numeric.ss` — Number parsing
- `parse-size str` — parse "10K", "5M", "2G" etc. with all GNU suffixes
  (b=512, kB=1000, K=1024, MB=10^6, M=2^20, GB=10^9, G=2^30, ...)
- `parse-count str` — parse leading +/- for relative counts (head/tail)
- `parse-integer str` — strict integer parsing with error
- `parse-float str` — floating-point parsing

### 0.6 — `common/permissions.ss` — Permission parsing
- `parse-mode str` — parse symbolic (u+rwx,g+rx) and octal (0755) modes
- `apply-mode base-mode parsed` — apply parsed mode to existing permissions

### 0.7 — `common/fileutils.ss` — File operation helpers
- `file-type path` — return type symbol
- `file-exists-or-die path` — check existence with error
- `copy-file src dst opts` — copy with attribute preservation
- `remove-file-or-dir path opts` — recursive remove

### 0.8 — `common/timeutils.ss` — Time parsing
- `parse-duration str` — parse sleep-style durations (Ns, Nm, Nh, Nd)
- `parse-timestamp str` — parse touch-style timestamps
- `format-time fmt time` — strftime-like formatting

### 0.9 — `common/version.ss` — Version info
- `coreutils-version` — version string
- `version-info program` — standard version output

### Tests
- `common-test.ss` — unit tests for all common modules

---

## Phase 1: Trivial Utilities (No/Minimal Options)

**Goal**: Implement the simplest utilities. Each is a few lines of logic.

### 1.1 — `true.ss` / `false.ss`
- `true`: exit 0. Recognize `--help` / `--version` if sole argument.
- `false`: exit 1. Same help/version handling.

### 1.2 — `yes.ss`
- Print "y" (or arguments joined by spaces) forever, one per line.
- Buffer output for performance (fill buffer, write in chunks).
- Exit on write error (broken pipe).

### 1.3 — `echo.ss`
- Options: `-n` (no newline), `-e` (enable escapes), `-E` (disable escapes).
- Escape sequences: `\\`, `\a`, `\b`, `\c`, `\e`, `\f`, `\n`, `\r`, `\t`, `\v`,
  `\0NNN` (octal), `\xHH` (hex).
- `\c` stops output immediately.
- Respect `POSIXLY_CORRECT` for default escape behavior.
- Only process `-n`/`-e`/`-E` flags; other `-...` strings are literal.

### 1.4 — `whoami.ss`
- Print effective username (Gambit `user-name` or `user-info (##get-euid)`).
- No options beyond `--help`/`--version`.

### 1.5 — `logname.ss`
- Print login name via `getenv "LOGNAME"` or Gambit equivalent.
- No options beyond `--help`/`--version`.

### 1.6 — `hostname.ss`
- No args: print hostname (`:std/os/hostname`).
- One arg: set hostname (requires root — FFI call to `sethostname`).

### 1.7 — `hostid.ss`
- Print 8-digit hex host ID (FFI call to `gethostid`).
- No options beyond `--help`/`--version`.

### 1.8 — `tty.ss`
- Print terminal device name or "not a tty".
- Option: `-s`/`--silent`/`--quiet` — no output, status only.
- Exit codes: 0=tty, 1=not-tty, 2=usage-error, 3=write-error, 4=ttyname-failed.

### 1.9 — `nproc.ss`
- Print number of available processors.
- Options: `--all` (all installed, ignore limits), `--ignore=N`.
- Respect `OMP_NUM_THREADS` / `OMP_THREAD_LIMIT` env vars.
- Result always >= 1.

### 1.10 — `printenv.ss`
- No args: print all environment variables (`get-environment-variables`).
- Args: print values of named variables.
- Option: `-0`/`--null` — NUL-terminated output.
- Exit 0 if all found, 1 if any missing.

### 1.11 — `sleep.ss`
- Parse duration arguments: `NUMBER[smhd]`, sum them.
- Floating-point supported.
- Use `thread-sleep!` or nanosleep equivalent.

### 1.12 — `pwd.ss`
- Options: `-L`/`--logical` (use $PWD), `-P`/`--physical` (resolve symlinks, default).
- Validate $PWD matches actual cwd for logical mode.

### 1.13 — `link.ss`
- Exactly 2 args: create hard link. Calls Gambit `create-link`.
- No options beyond `--help`/`--version`.

### 1.14 — `unlink.ss`
- Exactly 1 arg: remove file via `delete-file`.
- No options beyond `--help`/`--version`.

### 1.15 — `sync.ss`
- No args: global sync (FFI `sync()`).
- With files: `fsync()` each.
- Options: `-d`/`--data` (fdatasync), `-f`/`--file-system` (syncfs).

### Tests
- `trivial-test.ss` — test all Phase 1 utilities

---

## Phase 2: Simple Text Filters (Line-Oriented I/O)

**Goal**: Implement utilities that read lines and produce output with simple transforms.

### 2.1 — `cat.ss`
- Simple mode: copy stdin/files to stdout.
- Options: `-n` (number lines), `-b` (number non-blank), `-s` (squeeze blanks),
  `-E` (show ends as `$`), `-T` (show tabs as `^I`), `-v` (show non-printing),
  `-A` (= `-vET`), `-e` (= `-vE`), `-t` (= `-vT`).
- Treat `-` as stdin.
- Detect reading same file being written (error).

### 2.2 — `head.ss`
- Default: first 10 lines.
- Options: `-n NUM` (lines), `-c NUM` (bytes), `-q`/`-v` (headers),
  `-z` (NUL delimiter).
- Negative counts: `-n -5` means all but last 5 lines.
- Support size suffixes (K, M, G, etc.).
- Multiple files: print `==> filename <==` headers.

### 2.3 — `tail.ss`
- Default: last 10 lines.
- Options: `-n NUM`, `-c NUM`, `-q`/`-v`, `-z`, `--pid=PID`.
- `+NUM` means starting from line NUM.
- `-f`/`--follow` — output appended data as file grows.
- `--follow=name` vs `--follow=descriptor`.
- File rotation detection.

### 2.4 — `wc.ss`
- Options: `-l` (lines), `-w` (words), `-c` (bytes), `-m` (chars),
  `-L` (max line length).
- Default: lines, words, bytes.
- `--files0-from=FILE` — read filenames from NUL-delimited file.
- `--total=auto|always|only|never`.
- Right-aligned columnar output with shared width.

### 2.5 — `tee.ss`
- Copy stdin to stdout AND to files.
- Options: `-a` (append), `-i` (ignore SIGINT),
  `-p`/`--output-error[=MODE]` (warn/warn-nopipe/exit/exit-nopipe).

### 2.6 — `rev.ss`
- Reverse characters of each line.
- Note: `rev` is actually from util-linux, not coreutils. **SKIP** if not present
  in the source tree.

### 2.7 — `tac.ss`
- Concatenate and print files in reverse (last line first).
- Options: `-b` (attach separator before), `-r` (regex separator),
  `-s STRING` (use STRING as separator instead of newline).
- Handles large files with temp files.

### 2.8 — `nl.ss`
- Number lines of files.
- Options: `-b`/`-f`/`-h STYLE` (body/footer/header numbering: `a`/`t`/`n`/`pREGEX`),
  `-d CC` (section delimiter), `-v N` (start), `-i N` (increment),
  `-l N` (join blanks), `-s STRING` (separator), `-w N` (width),
  `-n FORMAT` (ln/rn/rz), `-p` (no renumber).
- Logical pages: header/body/footer sections delimited by `\:\:\:`, `\:\:`, `\:`.

### 2.9 — `fold.ss`
- Wrap long lines to specified width.
- Options: `-b` (bytes), `-s` (break at spaces), `-w WIDTH` (default 80).
- Tab handling: tabs expand to next tab stop (every 8).
- Backspace decrements column, CR resets to 0.

### 2.10 — `expand.ss` / `unexpand.ss`
- `expand`: Convert tabs to spaces.
  Options: `-i` (initial only), `-t TABLIST`.
- `unexpand`: Convert spaces to tabs.
  Options: `-a` (all blanks), `--first-only`, `-t TABLIST`.
- Tab stop list: single value or comma-separated positions.

### 2.11 — `fmt.ss`
- Reformat paragraph text.
- Options: `-w WIDTH` (default 75), `-c` (crown margin), `-t` (tagged paragraph),
  `-s` (split only), `-u` (uniform spacing), `-g GOAL` (goal width).
- Paragraph detection by indentation changes.
- Sentence-end detection (`.?!` followed by space).

### Tests
- One test file per utility (e.g., `cat-test.ss`, `head-test.ss`).

---

## Phase 3: Field/Column Processing

**Goal**: Implement utilities that process structured columnar/field data.

### 3.1 — `cut.ss`
- Options: `-b LIST` (bytes), `-c LIST` (chars), `-f LIST` (fields),
  `-d DELIM` (delimiter, default TAB), `-s` (only delimited),
  `--output-delimiter=STRING`, `--complement`, `-z`.
- LIST format: `N`, `N-M`, `N-`, `-M`, comma-separated.
- Exactly one of `-b`/`-c`/`-f` required.

### 3.2 — `paste.ss`
- Merge lines from multiple files side-by-side.
- Options: `-d LIST` (delimiter chars, cyclic), `-s` (serial), `-z`.
- Delimiter escapes: `\n`, `\t`, `\b`, `\f`, `\r`, `\v`, `\\`, `\0`.
- Parallel mode: combine corresponding lines.
- Serial mode: each file's lines joined on one output line.

### 3.3 — `join.ss`
- Join lines from two sorted files on a common field.
- Options: `-a FILENUM` (print unpairable), `-e EMPTY` (replace missing),
  `-1 FIELD` / `-2 FIELD` (join fields), `-t CHAR` (separator),
  `-i` (ignore case), `-o FORMAT` (output format),
  `--check-order`/`--nocheck-order`, `--header`, `-z`.

### 3.4 — `comm.ss`
- Compare two sorted files line by line.
- Options: `-1`/`-2`/`-3` (suppress columns), `--check-order`/`--nocheck-order`,
  `--output-delimiter=STR`, `--total`, `-z`.
- Three-column output: unique-to-file1, unique-to-file2, common.

### 3.5 — `sort.ss`
- Sort lines of text.
- Options: `-b` (ignore leading blanks), `-d` (dictionary order),
  `-f` (fold case), `-g` (general numeric), `-h` (human numeric),
  `-i` (ignore nonprinting), `-M` (month sort), `-n` (numeric),
  `-R` (random), `-r` (reverse), `-V` (version sort),
  `-k KEYDEF` (sort key), `-t SEP` (field separator),
  `-u` (unique), `-s` (stable), `-m` (merge), `-c`/`-C` (check order),
  `-o FILE` (output), `-T DIR` (temp dir), `-z`,
  `--parallel=N`, `--compress-program=PROG`.
- KEYDEF: `F[.C][OPTS][,F[.C][OPTS]]`.
- This is the most complex text utility. Implement basic sorting first,
  then add key-based and specialized sorts.

### 3.6 — `uniq.ss`
- Remove/report duplicate adjacent lines.
- Options: `-c` (count), `-d` (repeated), `-u` (unique),
  `-D[METHOD]` (all repeated), `--group[=METHOD]` (grouping),
  `-f N` (skip fields), `-s N` (skip chars), `-w N` (check chars),
  `-i` (ignore case), `-z`.

### 3.7 — `tr.ss`
- Translate, squeeze, or delete characters.
- Options: `-c`/`-C` (complement), `-d` (delete), `-s` (squeeze), `-t` (truncate).
- Character classes: `[:alnum:]`, `[:alpha:]`, `[:digit:]`, `[:lower:]`,
  `[:upper:]`, `[:space:]`, `[:blank:]`, `[:cntrl:]`, `[:graph:]`,
  `[:print:]`, `[:punct:]`, `[:xdigit:]`.
- Ranges: `a-z`. Escapes: `\NNN` (octal), `\xHH` (hex), `\n`, `\t`, etc.
- Equivalence classes: `[=c=]`. Repetition: `[c*N]`.

### 3.8 — `numfmt.ss`
- Convert numbers to/from human-readable format.
- Options: `--from=UNIT` (auto/si/iec/iec-i), `--to=UNIT`,
  `--from-unit=N`, `--to-unit=N`, `-d CHAR` (decimal point),
  `--field=FIELDS`, `--format=FORMAT`, `--header[=N]`,
  `--invalid=MODE`, `--padding=N`, `--round=METHOD`,
  `--suffix=SUFFIX`, `-z`.

### Tests
- One test file per utility.

---

## Phase 4: File & Directory Operations

**Goal**: Implement file manipulation utilities.

### 4.1 — `basename.ss`
- Strip directory and optional suffix from path.
- Options: `-a`/`--multiple`, `-s SUFFIX`, `-z`.
- Handle `//` on systems where distinct from `/`.

### 4.2 — `dirname.ss`
- Strip last component from path.
- Options: `-z`.
- No `/` in input → output `.`.

### 4.3 — `readlink.ss`
- Print symlink target.
- Options: `-f` (canonicalize), `-e` (canonicalize-existing),
  `-m` (canonicalize-missing), `-n` (no newline),
  `-q`/`-s` (quiet), `-v` (verbose), `-z`.

### 4.4 — `realpath.ss`
- Print resolved absolute path.
- Options: `-e` (all must exist), `-m` (no existence requirement),
  `-L` (logical), `-P` (physical), `-s` (no symlinks),
  `-q` (quiet), `--relative-to=DIR`, `--relative-base=DIR`, `-z`.

### 4.5 — `mkdir.ss`
- Create directories.
- Options: `-m MODE`, `-p` (parents), `-v` (verbose).

### 4.6 — `rmdir.ss`
- Remove empty directories.
- Options: `-p` (parents), `-v` (verbose), `--ignore-fail-on-non-empty`.

### 4.7 — `mktemp.ss`
- Create temporary file or directory safely.
- Options: `-d` (directory), `-q` (quiet), `-u` (dry-run),
  `--suffix=SUFF`, `-p DIR`/`--tmpdir[=DIR]`, `-t`.
- Template must contain 3+ consecutive `X`s.
- Default template: `tmp.XXXXXXXXXX`.

### 4.8 — `touch.ss`
- Update file timestamps or create empty file.
- Options: `-a` (access only), `-m` (modification only), `-c` (no create),
  `-d STRING` (date), `-r FILE` (reference), `-h` (no-dereference),
  `-t STAMP` (timestamp format).

### 4.9 — `ln.ss`
- Make links between files.
- Options: `-s` (symbolic), `-f` (force), `-i` (interactive),
  `-n` (no-dereference), `-b[CONTROL]` (backup), `-S SUFFIX`,
  `-t DIR` (target directory), `-T` (no target directory),
  `-v` (verbose), `-r` (relative), `-L`/`-P` (dereference/no).

### 4.10 — `cp.ss`
- Copy files and directories.
- Options: `-a` (archive), `-b` (backup), `-f` (force), `-i` (interactive),
  `-l` (hard link), `-L`/`-P`/`-H` (dereference modes), `-n` (no clobber),
  `-p` (preserve), `-R`/`-r` (recursive), `-s` (symbolic link),
  `-S SUFFIX`, `-t DIR`, `-T`, `-u[CONTROL]`, `-v`, `-x` (one filesystem),
  `--preserve=ATTR_LIST`, `--no-preserve=ATTR_LIST`,
  `--sparse=WHEN`, `--reflink=WHEN`.

### 4.11 — `mv.ss`
- Move/rename files.
- Options: `-b[CONTROL]` (backup), `-f` (force), `-i` (interactive),
  `-n` (no clobber), `-S SUFFIX`, `-t DIR`, `-T`, `-u[CONTROL]`, `-v`.
- Try rename() first; fall back to copy+delete.

### 4.12 — `rm.ss`
- Remove files/directories.
- Options: `-f` (force), `-i` (always prompt), `-I` (prompt once),
  `--interactive=WHEN`, `-r`/`-R` (recursive), `-d` (dir),
  `-v` (verbose), `--one-file-system`, `--preserve-root[=all]`.

### 4.13 — `install.ss`
- Copy files and set attributes.
- Options: `-b[CONTROL]`, `-c` (ignored), `-C` (compare), `-d` (directories),
  `-D` (create leading dirs), `-g GROUP`, `-m MODE`, `-o OWNER`,
  `-p` (preserve timestamps), `-s` (strip), `-S SUFFIX`, `-t DIR`,
  `-T`, `-v`.

### 4.14 — `shred.ss`
- Securely overwrite a file.
- Options: `-f` (force permissions), `-n N` (iterations, default 3),
  `-s N` (shred size), `-u[HOW]` (truncate+remove),
  `-v` (verbose), `-x` (don't round up), `-z` (final zero pass).

### Tests
- One test file per utility.

---

## Phase 5: File Information & Permissions

**Goal**: Implement utilities that inspect/modify file metadata.

### 5.1 — `ls.ss` (+ `dir.ss`, `vdir.ss`)
- List directory contents. **Most complex utility in coreutils.**
- Subphases:
  - 5.1a: Basic listing (one-per-line, simple columns)
  - 5.1b: Long format (`-l`) with permissions, owner, size, date
  - 5.1c: Sorting (`-t`, `-S`, `-r`, `-v`, etc.)
  - 5.1d: Recursive (`-R`) and filtering (`-a`, `-A`, `-d`)
  - 5.1e: Color output (`--color`)
  - 5.1f: Column formatting (`-C`, `-x`, `-m`, `-1`)
  - 5.1g: Full option set
- `dir` = `ls -C -b`, `vdir` = `ls -l -b`.

### 5.2 — `chmod.ss`
- Change file permissions.
- Options: `-R` (recursive), `-c` (changes), `-f` (silent), `-v` (verbose),
  `--preserve-root`/`--no-preserve-root`, `--reference=RFILE`.
- Symbolic mode: `[ugoa][+-=][rwxXst]...`
- Octal mode: `0755`, etc.

### 5.3 — `chown.ss` / `chgrp.ss`
- Change file owner/group.
- Options: `-R` (recursive), `-c`/`-f`/`-v`, `--dereference`/`--no-dereference`,
  `-H`/`-L`/`-P` (recursive dereference), `--preserve-root`,
  `--from=OWNER:GROUP`, `--reference=RFILE`.
- `chgrp` is `chown` restricted to group changes.

### 5.4 — `stat.ss`
- Display file or filesystem status.
- Options: `-L` (dereference), `-f` (filesystem), `-c FORMAT`/`--printf=FORMAT`,
  `-t` (terse).
- Format sequences: `%a` (access rights octal), `%A` (human readable),
  `%b` (blocks), `%d` (device), `%f` (raw mode hex), `%F` (type),
  `%g`/`%G` (gid/group name), `%h` (hardlinks), `%i` (inode),
  `%m` (mount point), `%n` (name), `%N` (quoted name/link target),
  `%o` (optimal I/O), `%s` (size), `%t`/`%T` (major/minor device),
  `%u`/`%U` (uid/user name), `%w`/`%W`/`%x`/`%X`/`%y`/`%Y`/`%z`/`%Z`
  (timestamps).

### 5.5 — `df.ss`
- Report filesystem disk space usage.
- Options: `-a` (all), `-B SIZE` (block size), `-h` (human readable),
  `-H` (si), `-i` (inodes), `-l` (local), `-P` (POSIX output),
  `-T` (type), `-t TYPE` (only type), `-x TYPE` (exclude type),
  `--total`, `--output[=FIELD_LIST]`, `--sync`/`--no-sync`.

### 5.6 — `du.ss`
- Estimate file space usage.
- Options: `-a` (all), `-B SIZE`, `-b` (bytes), `-c` (total),
  `-d DEPTH`, `-h` (human), `-H` (si), `-k`, `-l` (count hardlinks),
  `-m`, `-S` (separate dirs), `-s` (summary), `-x` (one filesystem),
  `--files0-from=FILE`, `--time`, `--time=WORD`, `--exclude=PATTERN`.

### 5.7 — `pathchk.ss`
- Check pathname validity and portability.
- Options: `-p` (POSIX), `-P` (POSIX + empty), `--portability`.

### Tests
- One test file per utility.

---

## Phase 6: Date, Time & Identity

**Goal**: Implement date/time and user/system identity utilities.

### 6.1 — `date.ss`
- Print or set the system date/time.
- Options: `-d STRING` (display STRING, not now), `-f FILE` (batch dates),
  `-I[FMT]` (ISO 8601), `-R` (RFC 5322), `-r FILE` (reference file),
  `-s STRING` (set date), `-u` (UTC), `--debug`.
- Format sequences: `%Y`, `%m`, `%d`, `%H`, `%M`, `%S`, `%N` (nanoseconds),
  `%Z` (timezone), `%A` (weekday), `%B` (month name), `%s` (epoch), etc.

### 6.2 — `id.ss`
- Print user/group IDs.
- Options: `-g` (group), `-G` (groups), `-n` (name), `-r` (real), `-u` (user),
  `-z` (zero-delimited).
- Default: `uid=N(name) gid=N(name) groups=N(name),...`.

### 6.3 — `groups.ss`
- Print group memberships (wrapper around id functionality).

### 6.4 — `who.ss`
- Show who is logged on.
- Options: `-a` (all), `-b` (boot time), `-d` (dead processes),
  `-H` (headers), `-l` (login processes), `-p` (active processes),
  `-q` (count), `-r` (runlevel), `-s` (short, default), `-t` (clock change),
  `-T`/`-w` (message status), `-u` (idle time).
- Reads `utmp` database.

### 6.5 — `users.ss`
- Print logged-in usernames (from utmp).

### 6.6 — `pinky.ss`
- Lightweight finger.
- Options: `-l` (long format), `-b` (no home dir),
  `-h` (no project file), `-p` (no plan file), `-s` (short format),
  `-f` (no heading), `-w` (no full name), `-i` (no idle time),
  `-q` (no name/tty/time).

### 6.7 — `uptime.ss`
- Show how long system has been running.
- Options: `-p` (pretty), `-s` (since boot).

### 6.8 — `uname.ss` / `arch.ss`
- Print system information.
- `uname` options: `-a` (all), `-s` (kernel name), `-n` (nodename),
  `-r` (release), `-v` (version), `-m` (machine), `-p` (processor),
  `-i` (hardware platform), `-o` (OS).
- `arch` = `uname -m`.

### Tests
- One test file per utility.

---

## Phase 7: Text Transforms & Encoding

**Goal**: Implement character/encoding transformation utilities.

### 7.1 — `seq.ss`
- Print number sequences.
- Options: `-f FORMAT` (printf), `-s SEPARATOR` (default newline), `-w` (equal width).
- Syntax: `seq LAST`, `seq FIRST LAST`, `seq FIRST INCR LAST`.
- Float support with precision detection.
- Fast path for integer-only sequences.

### 7.2 — `printf.ss`
- Format and print data.
- Format sequences: `%s`, `%d`, `%i`, `%o`, `%x`, `%X`, `%f`, `%e`, `%g`,
  `%c`, `%b` (string with backslash escapes), `%q` (shell-quoted).
- Backslash escapes: same as echo + `\uHHHH`, `\UHHHHHHHH`.
- Recycles format string if more args than placeholders.
- Width/precision modifiers: `%10s`, `%-10s`, `%.5s`, `%*d`.

### 7.3 — `expr.ss`
- Evaluate expressions.
- Operators: `|` (OR), `&` (AND), comparisons (`<`, `<=`, `=`, `!=`, `>=`, `>`),
  arithmetic (`+`, `-`, `*`, `/`, `%`), string matching (`:`, `match`, `substr`,
  `index`, `length`, `+` quote).
- All values are strings; numeric operations on integer-convertible strings.
- Exit: 0=true/nonzero, 1=false/zero/empty, 2=invalid, 3=internal.

### 7.4 — `test.ss` / `[.ss`
- Evaluate conditional expression.
- File tests: `-e`, `-f`, `-d`, `-r`, `-w`, `-x`, `-s`, `-L`, `-b`, `-c`,
  `-p`, `-S`, `-g`, `-u`, `-k`, `-G`, `-O`, `-N`, `-nt`, `-ot`, `-ef`.
- String tests: `-z`, `-n`, `=`, `!=`, `<`, `>`.
- Numeric tests: `-eq`, `-ne`, `-lt`, `-le`, `-gt`, `-ge`.
- Logical: `!`, `-a`, `-o`, `(`, `)`.
- `[` requires closing `]`.

### 7.5 — `basenc.ss` / `base64.ss` / `base32.ss`
- Encode/decode various base encodings.
- Options: `-d` (decode), `-i` (ignore garbage), `-w COLS` (wrap, default 76).
- `basenc` supports: `--base64`, `--base64url`, `--base32`, `--base32hex`,
  `--base16`, `--base2msbf`, `--base2lsbf`, `--z85`.

### 7.6 — `od.ss`
- Octal dump (and other formats).
- Options: `-A RADIX` (address), `-j BYTES` (skip), `-N BYTES` (read limit),
  `-t TYPE` (output type), `-v` (no duplicate suppression),
  `-w[BYTES]` (output width).
- Types: `a` (named char), `c` (printable/escape), `d`/`u`/`o`/`x` (int),
  `f` (float), with optional size suffixes.

### 7.7 — `cksum.ss` (+ `md5sum`, `sha*sum`, `b2sum`, `sum`)
- Compute checksums.
- Options (cksum): `-a ALGO` (crc/sm3/blake2b/md5/sha1/sha224/sha256/sha384/sha512),
  `-l LENGTH`, `--tag`, `--untagged`, `-b`, `-t`, `-z`.
- For md5sum/sha*sum: `-c` (check mode), `--quiet`, `--status`, `--strict`, `-w`.
- `sum`: `-r` (default, BSD), `-s` (System V).
- Use `:std/crypto` for hash algorithms where available.

### Tests
- One test file per utility.

---

## Phase 8: Process & System Control

**Goal**: Implement utilities that control processes and system state.

### 8.1 — `kill.ss`
- Send signals to processes.
- Options: `-s SIGNAL`, `-l`/`-L` (list signals), `-SIGNAL` (shorthand).
- Signal names: HUP, INT, QUIT, TERM, KILL, USR1, USR2, etc.
- Use `:std/os/signal` for signal constants and `kill` function.

### 8.2 — `env.ss`
- Run a command in a modified environment.
- Options: `-i` (empty environment), `-u VAR` (unset), `-0` (NUL output),
  `-C DIR` (chdir), `-S STRING` (split string), `-v` (verbose),
  `--default-signal=SIG`, `--ignore-signal=SIG`, `--block-signal=SIG`.
- No command: print environment.
- With command: exec with modified env.

### 8.3 — `nice.ss`
- Run with modified scheduling priority.
- Options: `-n ADJUST` (adjustment, default 10).
- Calls `nice()` syscall.

### 8.4 — `nohup.ss`
- Run command immune to hangups.
- Redirects stdout/stderr to `nohup.out` or `$HOME/nohup.out`.
- Sets SIGHUP to SIG_IGN.

### 8.5 — `timeout.ss`
- Run command with time limit.
- Options: `-k DURATION` (kill after), `-s SIGNAL` (default TERM),
  `--foreground`, `--preserve-status`, `-v` (verbose).
- Exit 124 if timed out.

### 8.6 — `stdbuf.ss`
- Run command with modified I/O buffering.
- Options: `-i MODE` (stdin), `-o MODE` (stdout), `-e MODE` (stderr).
- Modes: `L` (line), `0` (unbuffered), `SIZE` (buffered).
- Requires preloading `libstdbuf.so` via `LD_PRELOAD`.

### 8.7 — `chroot.ss`
- Run command in a different root directory.
- Options: `--userspec=USER:GROUP`, `--groups=GROUPS`, `--skip-chdir`.
- Calls `chroot()` syscall.

### Tests
- One test file per utility.

---

## Phase 9: Advanced File Operations

**Goal**: Implement complex file processing utilities.

### 9.1 — `dd.ss`
- Convert and copy a file with optional transformations.
- Operands: `if=FILE`, `of=FILE`, `bs=N`, `ibs=N`, `obs=N`, `count=N`,
  `skip=N`, `seek=N`, `conv=CONVS`, `iflag=FLAGS`, `oflag=FLAGS`,
  `status=LEVEL`.
- Conversions: `ascii`, `ebcdic`, `ibm`, `block`, `unblock`, `lcase`,
  `ucase`, `sparse`, `swab`, `sync`, `excl`, `nocreat`, `notrunc`,
  `noerror`, `fdatasync`, `fsync`.
- Print statistics on SIGUSR1 and completion.

### 9.2 — `split.ss`
- Split a file into pieces.
- Options: `-a N` (suffix length), `-b SIZE` (bytes), `-C SIZE` (line bytes),
  `-d`/`-x` (numeric/hex suffixes), `-e` (no empty files),
  `-l N` (lines), `-n CHUNKS` (number of chunks),
  `--additional-suffix=SUFFIX`, `--filter=COMMAND`,
  `--numeric-suffixes[=FROM]`, `--hex-suffixes[=FROM]`,
  `-t SEP`, `--verbose`.

### 9.3 — `csplit.ss`
- Split by context (pattern matching).
- Options: `-b FORMAT` (suffix format), `-f PREFIX` (default `xx`),
  `-k` (keep on error), `-n DIGITS`, `-z` (remove empty), `-s` (quiet).
- Patterns: `INTEGER`, `/REGEX/[OFFSET]`, `%REGEX%[OFFSET]`, `{REPEAT}`.

### 9.4 — `truncate.ss`
- Shrink or extend file size.
- Options: `-c` (no create), `-o` (use io blocks), `-r FILE` (reference),
  `-s SIZE` (size with +/-/< />/ % modifiers).

### 9.5 — `mkfifo.ss`
- Create named pipe (FIFO).
- Options: `-m MODE`.

### 9.6 — `mknod.ss`
- Create special file.
- Options: `-m MODE`.
- Types: `b` (block), `c`/`u` (character), `p` (FIFO).

### 9.7 — `dircolors.ss`
- Generate LS_COLORS configuration.
- Options: `-b` (Bourne shell), `-c` (C shell), `-p` (print defaults).
- Reads `.dircolors` database file.

### Tests
- One test file per utility.

---

## Phase 10: Text Layout & Printing

**Goal**: Implement complex text formatting utilities.

### 10.1 — `pr.ss`
- Paginate/columnate files for printing.
- Options: `+FIRST_PAGE[:LAST_PAGE]`, `-COLUMN`, `-a` (across), `-c` (show ctrl),
  `-d` (double space), `-D FORMAT` (date format), `-e[CHAR[WIDTH]]`,
  `-f`/`-F` (form feed), `-h HEADER`, `-i[CHAR[WIDTH]]`,
  `-J` (join lines), `-l LINES` (page length), `-m` (merge files),
  `-n[SEP[DIGITS]]`, `-o MARGIN`, `-r` (no missing file warnings),
  `-s[CHAR]`, `-S[STRING]`, `-t` (no headers/trailers),
  `-T` (no headers/trailers/form feeds), `-v` (show nonprinting),
  `-w WIDTH`, `-W WIDTH`.

### 10.2 — `ptx.ss`
- Produce permuted index.
- Options: `-A` (auto reference), `-b FILE` (break chars),
  `-f` (fold case), `-F` (right/left output), `-g N` (gap),
  `-G` (GNU formatting), `-i FILE` (ignore words),
  `-o FILE` (only words), `-r` (regex), `-R` (right context),
  `-S REGEX` (sentence end), `-T FORMAT` (TeX), `-w N` (width),
  `-W STRING`.

### 10.3 — `tsort.ss`
- Topological sort.
- Reads pairs of strings representing partial ordering.
- Detects cycles and prints warning.

### 10.4 — `shuf.ss`
- Generate random permutations.
- Options: `-e` (treat args as lines), `-i LO-HI` (range),
  `-n COUNT`, `-o FILE`, `-z`, `--random-source=FILE`, `-r` (repeat).

### 10.5 — `factor.ss`
- Print prime factors.
- Supports arbitrarily large numbers (bignum).
- Pollard's rho algorithm for factorization.

### 10.6 — `stty.ss`
- Get/set terminal settings.
- Options: `-a` (all), `-g` (save form), `-F DEVICE`.
- Settings: ispeed, ospeed, rows, columns, and many termios flags.
- Most complex terminal utility. Uses termios FFI.

### Tests
- One test file per utility.

---

## Phase 11: Security Context Utilities

**Goal**: Implement SELinux/SMACK context utilities (may be stubs if no SELinux).

### 11.1 — `chcon.ss`
- Change file SELinux security context.

### 11.2 — `runcon.ss`
- Run command with specified SELinux security context.

### Note
These utilities are Linux-specific and require SELinux libraries. Implementation
will check for SELinux availability and provide error messages if unavailable.

---

## Phase 12: Multicall Binary & Polish

**Goal**: Create a unified multicall binary and final polish.

### 12.1 — Multicall binary
- `coreutils.ss` — dispatch to individual utilities based on `argv[0]`.
- Pattern: `(case (path-strip-directory (car (command-line))) ...)`

### 12.2 — Performance optimization
- Profile critical paths (cat, sort, wc) and optimize.
- Ensure large file handling doesn't blow memory.

### 12.3 — Integration test suite
- End-to-end tests comparing gerbil-coreutils output vs GNU coreutils.
- Script that runs both implementations on same inputs and diffs output.

### 12.4 — Documentation
- README with build instructions, supported utilities, known differences.
- Man page generation or `--help` text matching GNU format.

---

## Implementation Priority & Complexity Ratings

| Phase | Utilities | Count | Complexity | Effort |
|-------|-----------|-------|------------|--------|
| 0 | Infrastructure | 8 modules | Medium | High (foundational) |
| 1 | true,false,yes,echo,whoami,logname,hostname,hostid,tty,nproc,printenv,sleep,pwd,link,unlink,sync | 16 | Trivial-Simple | Low |
| 2 | cat,head,tail,wc,tee,tac,nl,fold,expand,unexpand,fmt | 11 | Medium | Medium |
| 3 | cut,paste,join,comm,sort,uniq,tr,numfmt | 8 | Medium-High | High |
| 4 | basename,dirname,readlink,realpath,mkdir,rmdir,mktemp,touch,ln,cp,mv,rm,install,shred | 14 | Medium-High | Very High |
| 5 | ls,chmod,chown,chgrp,stat,df,du,pathchk | 8 | High-Very High | Very High |
| 6 | date,id,groups,who,users,pinky,uptime,uname,arch | 9 | Medium | Medium |
| 7 | seq,printf,expr,test/[,basenc,base64,base32,od,cksum,md5sum,sha*sum,b2sum,sum | 14 | Medium-High | High |
| 8 | kill,env,nice,nohup,timeout,stdbuf,chroot | 7 | Medium | Medium |
| 9 | dd,split,csplit,truncate,mkfifo,mknod,dircolors | 7 | High | High |
| 10 | pr,ptx,tsort,shuf,factor,stty | 6 | High-Very High | High |
| 11 | chcon,runcon | 2 | Low (stubs) | Low |
| 12 | Multicall + polish | — | Medium | Medium |
| **Total** | | **~109** | | |

---

## Dependencies & Build Order

```
Phase 0 (infrastructure) ─── must complete first
  │
  ├── Phase 1 (trivial) ──── can start immediately after Phase 0
  │
  ├── Phase 2 (text filters) ── depends on common/io.ss
  │     │
  │     └── Phase 3 (field processing) ── builds on Phase 2 patterns
  │
  ├── Phase 4 (file ops) ── depends on common/fileutils.ss, common/permissions.ss
  │     │
  │     └── Phase 5 (file info) ── builds on Phase 4
  │
  ├── Phase 6 (date/identity) ── depends on common/timeutils.ss
  │
  ├── Phase 7 (transforms) ── mostly independent
  │
  ├── Phase 8 (process control) ── depends on signal handling
  │
  ├── Phase 9 (advanced file) ── depends on Phase 4
  │
  ├── Phase 10 (text layout) ── depends on Phase 2, Phase 3
  │
  └── Phase 11 (security) ── mostly independent
        │
        └── Phase 12 (multicall + polish) ── depends on ALL phases
```

---

## FFI Requirements

Some utilities require C FFI calls not available in pure Gerbil:

| Function | Used By | Gerbil Equivalent |
|----------|---------|-------------------|
| `gethostid()` | hostid | FFI needed |
| `sethostname()` | hostname (set mode) | FFI needed |
| `sync()` / `fsync()` / `fdatasync()` | sync | FFI needed (partially in `:std/os/fd`) |
| `chroot()` | chroot | FFI needed |
| `nice()` | nice | FFI needed |
| `utmp` reading | who, users, uptime, pinky | FFI needed |
| `termios` | stty | FFI needed |
| `statvfs()` | df | FFI needed |
| `nftw()` or manual walk | du, chmod -R, chown -R | Manual implementation |
| `lchown()` | chown | FFI needed |
| `mkfifo()` | mkfifo | FFI needed |
| `mknod()` | mknod | FFI needed |
| `ttyname()` | tty | Gambit may provide |
| `sysconf(_SC_NPROCESSORS_ONLN)` | nproc | FFI needed |
| `uname()` | uname | FFI needed |

A shared `ffi.ss` module will provide these C bindings via Gambit's `begin-ffi`.

---

## Risk Factors

1. **Sort complexity**: GNU sort is ~4000 lines with multi-threading, temp file
   compression, and SIMD. The Gerbil version will be simpler but must produce
   identical output for the same inputs and locale.

2. **ls complexity**: GNU ls is ~5000 lines. Full color and format support is
   significant. Plan for incremental implementation.

3. **Locale handling**: GNU coreutils is deeply locale-aware (LC_COLLATE, LC_CTYPE).
   Gerbil has limited locale support — may need FFI for `strcoll()`.

4. **utmp access**: Required for who/users/uptime/pinky. Linux-specific binary
   format requires FFI.

5. **Performance**: Some utilities (cat, wc, sort) use SIMD and kernel-level
   optimizations. Gerbil versions will be slower but functionally equivalent.

6. **SELinux**: chcon/runcon require libselinux. Will be conditional/stub.

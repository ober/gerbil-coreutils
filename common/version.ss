(export
  coreutils-version
  version-info)

(import :gerbil-coreutils/common)

(def coreutils-version "0.1.0")

;; Print standard version output
(def (version-info program)
  (displayln program " (gerbil-coreutils) " coreutils-version))

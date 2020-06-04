(spacemacs/declare-prefix "a C" "Calibre")

(spacemacs/set-leader-keys
  "a C c" 'calibredb
  "a C l" 'calibredb-list
  )

(when (package-installed-p 'helm)
  (spacemacs/set-leader-keys
    "a C f" 'calibredb-find-helm))

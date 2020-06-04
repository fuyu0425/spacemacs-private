;;; packages.el --- leo-calibre layer packages file for Spacemacs.

;;; Code:

(defconst leo-calibre-packages
  '(calibredb)
)

(defun leo-calibre/init-calibredb ()
  `(use-package calibredb
    :commands (calibredb calibredb-list ,(when (package-installed-p 'helm) 'calibredb-helm))
    )
  )

;;; packages.el ends here

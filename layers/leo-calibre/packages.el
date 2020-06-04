;;; packages.el --- leo-calibre layer packages file for Spacemacs.

;;; Code:

(defconst leo-calibre-packages
  '(calibredb)
)

(defun leo-calibre/init-calibredb ()
  (eval `(use-package calibredb
    :commands (calibredb calibredb-list ,(when (package-installed-p 'helm) 'calibredb-find-helm))
    ))
  )

;;; packages.el ends here

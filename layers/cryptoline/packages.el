;;; packages.el --- cryptoline layer packages file for Spacemacs.
;;; Code:

(defconst cryptoline-packages
  '((cryptoline-mode :location local))
)

(defun cryptoline/init-cryptoline-mode ()
  (use-package cryptoline-mode
    :mode ("\\.cl\\'")
    )
  )

;;; packages.el ends here

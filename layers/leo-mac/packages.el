;;; packages.el --- leo-mac layer packages file for Spacemacs.
;;; Code:

(defconst leo-mac-packages
  '(
    (magit-autofetch :location local)
    )
)

(defun leo-mac/init-magit-autofetch ()
  (use-package magit-autofetch
    :commands (magit-autofetch-mode))
  )

;;; packages.el ends here

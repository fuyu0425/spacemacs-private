;;; packages.el --- leo-misc layer packages file for Spacemacs.
;;; Code:

(defconst leo-misc-packages
 '(direnv)
)

(defun leo-misc/init-direnv()
  (use-package direnv
    :demand t
    :config
    (direnv-mode)
    (setq direnv-always-show-summary nil)
    :hook
    ((prog-mode) . direnv--maybe-update-environment))
  )

;;; packages.el ends here

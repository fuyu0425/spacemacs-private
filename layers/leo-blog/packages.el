;;; packages.el --- leo-blog layer packages file for Spacemacs.
;;; Code:

(defconst leo-blog-packages
  '(
    easy-hugo
    )
)

(defun leo-blog/init-easy-hugo ()
  (use-package easy-hugo
    :after (helm-ag)
    :ensure t)
  )


;;; packages.el ends here

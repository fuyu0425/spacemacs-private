;;; packages.el --- Coq Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|define-jump-handlers coq-mode)
(add-hook 'coq-mode-hook
          (lambda ()
            (setq-local projectile-tags-backend 'etags-select)
            (setq-local dash-at-point-docset "Coq")
            ))
(setq spacemacs-jump-handlers-coq-mode nil)

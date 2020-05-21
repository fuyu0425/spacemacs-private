;;; packages.el --- coq layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Jeremy Bi <bixuanxbi@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq leo-coq-packages
      '(
        (company-coq :requires company)
        proof-general
        smartparens
        vi-tilde-fringe
        ))

(defun leo-coq/init-company-coq ()
  (use-package company-coq
    :defer t
    :init
    (progn
      (add-hook 'coq-mode-hook #'company-coq-mode)
      (add-to-list 'spacemacs-jump-handlers-coq-mode
                   'company-coq-jump-to-definition)
      (setq company-coq-disabled-features '(hello
                                            ;; inline-docs
                                            ;; company
                                            spinner)))
    :config
    (progn
      (spacemacs|hide-lighter company-coq-mode)
      (dolist (prefix '(("mi" . "coq/insert")
                        ("mh" . "coq/document")))
        (spacemacs/declare-prefix-for-mode
          'coq-mode
          (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'coq-mode
        "il" 'company-coq-lemma-from-goal
        "im" 'company-coq-insert-match-construct
        "ao" 'company-coq-occur
        "he" 'company-coq-document-error
        "hE" 'company-coq-browse-error-messages
        "hh" 'company-coq-doc))
    (progn
      (add-hook 'company-coq-mode-hook #'company-mode)
      (add-hook 'company-coq-mode-hook
                (lambda ()
                  (add-to-list 'company-backends
                               '(company-coq-math-symbols-backend
                                 company-coq-choices-backend company-coq-master-backend
                                 :with company-etags :with company-yasnippet))
                  (setq-local company-transformers '(spacemacs//company-transformer-cancel company-sort-by-backend-importance))
                  ))
      )
    ))

(defun leo-coq/setup-coq-keys ()
  (evil-define-key 'normal coq-mode-map
    (kbd "M-l") 'proof-goto-point
    (kbd "M-k") 'proof-undo-last-successful-command
    (kbd "M-j") 'proof-assert-next-command-interactive
    (kbd "<f7>") 'proof-undo-last-successful-command
    (kbd "<f8>") 'proof-assert-next-command-interactive
    (kbd "<f9>") 'proof-goto-point
    )
  (evil-define-key 'insert coq-mode-map
    (kbd "M-l") 'proof-goto-point
    (kbd "M-k") 'proof-undo-last-successful-command
    (kbd "M-j") 'proof-assert-next-command-interactive
    (kbd "<f7>") 'proof-undo-last-successful-command
    (kbd "<f8>") 'proof-assert-next-command-interactive
    (kbd "<f9>") 'proof-goto-point
    )
  )


(defun leo-coq/init-proof-general ()
  (use-package proof-site
    :mode ("\\.v\\'" . coq-mode)
    :defer t
    :init
    (progn
      (setq leo-coq/proof-general-load-path
            (concat (configuration-layer/get-elpa-package-install-directory
                     'proof-general) "generic")
            proof-three-window-mode-policy 'hybrid
            proof-script-fly-past-comments t
            proof-splash-seen t
            )
      (add-to-list 'load-path leo-coq/proof-general-load-path))
    :config
    (progn
      (spacemacs|hide-lighter holes-mode)
      (spacemacs|hide-lighter proof-active-buffer-fake-minor-mode)
      ;; key bindings
      (dolist (prefix '(("ml" . "pg/layout")
                        ("mP" . "pg/prover")
                        ("ma" . "pg/ask-prover")
                        ("mai" . "show-implicits")
                        ("mg" . "pg/goto")
                        ("mi" . "coq/insert")
                        ("mh" . "coq/document")))
        (spacemacs/declare-prefix-for-mode
         'coq-mode
         (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'coq-mode
        ;; Basic proof management
        "n" 'proof-assert-next-command-interactive
        "]" 'proof-assert-next-command-interactive
        "u" 'proof-undo-last-successful-command
        "[" 'proof-undo-last-successful-command
        ;; "." 'proof-goto-point
        ;; Layout
        "lc" 'pg-response-clear-displays
        "ll" 'proof-layout-windows
        "lp" 'proof-prf
        ;; Prover Interaction
        "Pi" 'proof-interrupt-process
        "Pp" 'proof-process-buffer
        "x" 'proof-shell-exit
        "Pq" 'proof-shell-exit
        "Pr" 'proof-retract-buffer
        ;; Prover queries ('ask prover')
        "aa" 'coq-About
        "aA" 'coq-About-with-all
        "p"  'coq-Print
        "ap" 'coq-Print
        "aP" 'coq-Print-with-all
        "?"  'coq-Check
        "ac" 'coq-Check
        "aC" 'coq-Check-show-all
        "s"  'proof-find-theorems
        "af" 'proof-find-theorems
        "aib" 'coq-About-with-implicits
        "aic" 'coq-Check-show-implicits
        "aii" 'coq-Print-with-implicits
        ;; Moving the point (goto)
        "ge" 'proof-goto-command-end
        "gl" 'proof-goto-end-of-locked
        "gs" 'proof-goto-command-start
        ;; Insertions
        "ic" 'coq-insert-command
        "ie" 'coq-end-Section
        "ii" 'coq-insert-intros
        "ir" 'coq-insert-requires
        "is" 'coq-insert-section-or-module
        "it" 'coq-insert-tactic
        "iT" 'coq-insert-tactical
        (or dotspacemacs-major-mode-leader-key ",") 'proof-goto-point)
      (leo-coq/setup-coq-keys)
      (setq coq-one-command-per-line nil
            coq-mode-abbrev-table '()
            coq-terms-abbrev-table '())
      )))

(defun leo-coq/post-init-smartparens ()
  (spacemacs/add-to-hooks (if dotspacemacs-smartparens-strict-mode
                              'smartparens-strict-mode
                            'smartparens-mode)
                          '(coq-mode-hook)))

(defun leo-coq/post-init-vi-tilde-fringe ()
  (spacemacs/add-to-hooks 'spacemacs/disable-vi-tilde-fringe
                          '(coq-response-mode-hook
                            coq-goals-mode-hook)))

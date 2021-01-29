;;; packages.el --- leo-org layer packages file for Spacemacs.
;;; Code:

(defconst leo-org-packages
  '(
    org
    org-agenda
    org-super-agenda
    org-journal
    doct
    (ox-extra :location built-in)
    org-special-block-extras
    )
  )

(defun leo-org/post-init-org ()
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-protocol t)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "iP"  'leo/org-insert-clipboard)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook (lambda () (setq-local org-download-image-dir
                                                  (concat (file-name-sans-extension (buffer-name))
                                                          "_images"))))
  (setq org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)

  ;; skil some
  (setq  org-agenda-skip-scheduled-if-done t
         org-agenda-skip-deadline-if-done t
         org-agenda-skip-timestamp-if-done t)

  (setq org-log-done 'time
        org-log-into-drawer t
        )

  (setq org-todo-keywords
        '((type "INBOX(I)" "WORK(W)" "STUDY(S)" "LEISURE(L)" "|")
          (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "RECORD(r@)" "ABORT(a@/!)")
          ))

  (setq org-todo-repeat-to-state "NEXT")

  (setq org-todo-keyword-faces
        '(("WORK" .      (:background "red" :foreground "white" :weight bold))
          ("STUDY" .      (:background "white" :foreground "red" :weight bold))
          ("LEISURE" .      (:foreground "MediumBlue" :weight bold))
          ("TODO" .      (:background "DarkOrange" :foreground "black" :weight bold))
          ;; ("STARTED" .      (:background "red" :foreground "white" :weight bold))
          ("DONE" .      (:background "azure" :foreground "Darkgreen" :weight bold))
          ("RECORD" .      (:background "blue" :foreground "Darkgreen" :weight bold))
          ("ABORT" .     (:background "gray" :foreground "black"))
          ))

  (setq org-highest-priority ?A
        org-lowest-priority  ?E
        org-default-priority ?E)

  (setq org-priority-faces
        '((?A . (:background "red" :foreground "white" :weight bold))
          (?B . (:background "DarkOrange" :foreground "white" :weight bold))
          (?C . (:background "yellow" :foreground "DarkGreen" :weight bold))
          (?D . (:background "DodgerBlue" :foreground "black" :weight bold))
          (?E . (:background "SkyBlue" :foreground "black" :weight bold))
          ))

  (setq org-tag-alist '(
                        ("Work" . ?w)
                        ("@Sinica" . ?s)
                        ("@Home" . ?h)
                        ))
  (setq org-clock-out-remove-zero-time-clocks t)
  )

(defun leo-org/post-init-org-agenda ()
  (setq org-agenda-files '(
                           "~/org/gtd/inbox.org"
                           "~/org/gtd/gtd.org"
                           "~/org/gtd/tickler.org"
                           ;; "~/org/gtd/finished.org"
                           )
        org-agenda-start-day nil ; 1day
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-log-mode-items '(closed clock state))

  (with-eval-after-load 'org-projectile
    (mapcar (lambda (file)
              (when (file-exists-p file)
                (push file org-agenda-files)))
            (org-projectile-todo-files)))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)
                             ("~/org/gtd/someday.org" :maxlevel . 3)
                             ("~/org/gtd/finished.org" :maxlevel . 3)
                             )
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file)
  )

(defun leo-org/init-doct ()
  (use-package doct
    :ensure t
    ;;recommended: defer until calling doct
    :commands (doct))
  )

(defun leo-org/post-init-doct ()
  (setq org-capture-templates
        (doct '(
                ("Appointment" :keys "a"
                 :file "~/org/gtd/tickler.org"
                 :headline "Reminder"
                 :todo-state "NEXT"
                 :template "* %{todo-state} %?\n %^{Appointment DateTime}T %i\n")
                ("GTD Inbox" :keys "i"
                 :file "~/org/gtd/inbox.org"
                 :template "* INBOX %?\n  %i\n"
                 )
                ("Journal Entry" :keys "j"
                 :function org-journal-find-location
                 :template "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
                ("Note" :keys "n"
                 :file "~/org/notes.org"
                 :template "* %?\n  %u\n  %a")
                ("Code Snippets" :keys "s"
                 :file "~/org/snippets.org"
                 :template "* %?\t%^g\n#+BEGIN_SRC %^{language}\n%i\n#+END_SRC"
                 )
                ("Template for org-protocol" :keys "z"
                 :file "~/org/gtd/inbox.org"
                 :todo-state "INBOX"
                 :immediate-finish t
                 :children (("Link" :keys "l"
                             :template "* %{todo-state} %:annotation\n%i"
                             :kill-buffer t
                             ))))))
  )

(defun leo-org/init-org-super-agenda ()
  (use-package org-super-agenda
    :config
    (add-hook 'org-agenda-mode-hook 'org-super-agenda-mode))
  )

(defun leo-org/post-init-org-super-agenda ()
  (setq org-super-agenda-groups
        '( ;; Each group has an implicit boolean OR operator between its selectors.
          (:name "Today"          ; Optionally specify section name
                 :date today
                 )
          (:name "Important" :priority "A")
          (:priority<= "B"
                       ;; Show this section after "Today" and "Important", because
                       ;; their order is unspecified, defaulting to 0. Sections
                       ;; are displayed lowest-number-first.
                       :order 1)
          (:name "Habits" :habit t :order 2)
          ;; (:name "Shopping" :tag "Besorgung" :order 3)
          ;; Boolean AND group matches items that match all subgroups
          ;;  :and (:tag "shopping" :tag "@town")
          ;; Multiple args given in list with implicit OR
          ;;  :tag ("food" "dinner"))
          ;;  :habit t
          ;;  :tag "personal")
          ;; (:name "Started" :todo "STARTED" :order 5)
          ;;(:name "Space-related (non-moon-or-planet-related)"
          ;;       ;; Regexps match case-insensitively on the entire entry
          ;;       :and (:regexp ("space" "NASA")
          ;;                     ;; Boolean NOT also has implicit OR between selectors
          ;;                     :not (:regexp "moon" :tag "planet")))
          ;; (:name "BWG" :tag "@BWG" :order 7)
          ;; (:todo "WAITING" :order 9)    ; Set order of this section
          ;; (:name "read" :tag "2read" :order 15)
          ;; Groups supply their own section names when none are given
          ;; (:todo ("SOMEDAY" "WATCHING")
          ;; Show this group at the end of the agenda (since it has the
          ;; highest number). If you specified this group last, items
          ;; with these todo keywords that e.g. have priority A would be
          ;; displayed in that group instead, because items are grouped
          ;; out in the order the groups are listed.
          ;; :order 25)
          ;; (:name "reward"
          ;;        :tag ("reward" "lp")
          ;;        :order 100)
          ;; After the last group, the agenda will display items that didn't
          ;; match any of these groups, with the default order position of 99
          ))
  )

(defun leo-org/post-init-org-journal ()
  (setq org-journal-dir "~/org/journal/"
        org-journal-file-format "%Y-%m-%d"
        org-journal-date-prefix "#+TITLE: "
        org-journal-date-format "%A, %B %d %Y"
        org-journal-time-prefix "* "
        org-journal-time-format "")

  (setq org-journal-enable-agenda-integration t
        org-icalendar-store-UID t
        org-icalendar-include-todo "all"
        org-icalendar-combined-agenda-file "~/org/org-calendar.ics")
  )

(defun leo-org/init-ox-extra ()
  (use-package ox-extra :after ox
    :config
    (ox-extras-activate '(ignore-headlines))
    ))


(defun leo-org/init-org-special-block-extras ()
  (use-package org-special-block-extras
    :ensure t
    :hook (org-mode . org-special-block-extras-mode)
    ;; :custom
    ;; (org-special-block-extras--docs-libraries
    ;;  '("~/org-special-block-extras/documentation.org")
    ;;  "The places where I keep my ‘#+documentation’")
    ;; (org-special-block-extras-fancy-links
    ;; nil "Disable this feature.")
    :config
    ;; Use short names like ‘defblock’ instead of the fully qualified name
    ;; ‘org-special-block-extras--defblock’
    (org-special-block-extras-short-names))
  )

;;; packages.el ends here

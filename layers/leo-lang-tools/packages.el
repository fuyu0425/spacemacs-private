(defconst leo-lang-tools-packages
  '((mw-thesaurus
     :location (recipe :fetcher github :repo "agzam/mw-thesaurus.el")
     ;; local
     )

    ;; sdcv-mode is for browsing Stardict format dictionaries in Emacs
    ;;
    ;; to get Webster’s Revised Unabridged Dictionary
    ;; 1) download it from https://s3.amazonaws.com/jsomers/dictionary.zip
    ;; 2) unzip it twice and put into ~/.stardict/dic
    ;; 3) Install sdcv, a command-line utility for accessing StarDict dictionaries
    ;;
    ;; you can find more dicts in stardict format here: http://download.huzheng.org/dict.org/
    ;; don't get the package from MELPA - it's been reported broken
    (sdcv-mode :location (recipe
                          :fetcher github
                          :repo "gucong/emacs-sdcv"))
    (emacs-grammarly :location (recipe
                                :fetcher github
                                :repo "mmagnus/emacs-grammarly"))))

(defun leo-lang-tools/init-mw-thesaurus ()
  (use-package mw-thesaurus
    :demand t
    :config
    (define-key mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
    (spacemacs/set-leader-keys
      "xlm" #'mw-thesaurus-lookup-at-point
      "xAg" #'add-global-abbrev
      "xAl" #'add-mode-abbrev)))

(defun leo-lang-tools/region-or-word-at-point-str ()
  "Returns string of selected region or word at point"
  (let* ((bds (if (use-region-p)
                  (cons (region-beginning) (region-end))
                (bounds-of-thing-at-point 'word)))
         (p1 (car bds))
         (p2 (cdr bds)))
    (buffer-substring-no-properties p1 p2)))

(defun leo-lang-tools/init-sdcv-mode ()
  (use-package sdcv-mode
    :demand t
    :config
    (add-hook 'sdcv-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

    (defun sdcv-search-at-point ()
      (interactive)
      (sdcv-search (leo-lang-tools/region-or-word-at-point-str)
                   nil
                   '("朗道英漢字典5.0"
                    "朗道漢英字典5.0"
                    "dictd_www.dict.org_gcide"
                    "Moby Thesaurus II"
                    "English Etymology"
                    )
                   t))

    (defun sdcv-search-input ()
      (interactive)
      (let* (
             (word (read-string "Word to search: "))
             )
        (sdcv-search word
                     nil
                     '("朗道英漢字典5.0"
                       "朗道漢英字典5.0"
                       "dictd_www.dict.org_gcide"
                       "Moby Thesaurus II"
                       "English Etymology"
                       )
                     t)))
    (spacemacs/set-leader-keys "xll" #'sdcv-search-at-point)
    (spacemacs/set-leader-keys "xlL" #'sdcv-search-input)

    (evil-define-key 'normal sdcv-mode-map "q" #'sdcv-return-from-sdcv)
    (evil-define-key 'normal sdcv-mode-map "n" #'sdcv-next-entry)
    (evil-define-key 'normal sdcv-mode-map "p" #'sdcv-previous-entry)
    (evil-define-key 'normal sdcv-mode-map (kbd "RET") #'sdcv-search-at-point)
    (evil-define-key 'normal sdcv-mode-map "a" #'sdcv-search-at-point)
    (evil-global-set-key 'normal "T" #'sdcv-search-at-point)))

(defun leo-lang-tools/init-emacs-grammarly ()
  (use-package emacs-grammarly
    :config
    (spacemacs/set-leader-keys
      "xlg" #'grammarly-save-region-and-run)))

;; (setq ispell-program-name "aspell")

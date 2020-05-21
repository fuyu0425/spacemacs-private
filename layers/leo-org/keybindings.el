(spacemacs/set-leader-keys
  "aoBn" 'org-brain-deft
  )
(with-eval-after-load 'org-clock
            (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
            (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

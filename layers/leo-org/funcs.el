(defun leo/org-insert-clipboard ()
  (interactive)
  (let* (
         (image-file (concat
                      (org-download--dir)
                      "/"
                      (format-time-string "%Y%m%d_%H%M%S_.png"))))
         (org-download-clipboard)
    ;;      (exit-status
    ;;       (call-process "pngpaste" nil nil nil
    ;;                     image-file)))
    ;; (insert (if (= org-download-image-org-width 0)
    ;;             ""
    ;;           (format "#+attr_org: :width %dpx\n" org-download-image-org-width)))
    ;; (org-insert-link nil (concat "file:" image-file) "")
    (org-display-inline-images)))

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(defun org-brain-deft ()
  "Use `deft' for files in `org-brain-path'."
  (interactive)
  (let ((deft-directory (cond ((boundp 'org-brain-path) org-brain-path) (t "~/org/brain")))
        (deft-recursive t)
        (deft-extensions '("org")))
    (deft)))


;; source: http://kitchingroup.cheme.cmu.edu/blog/2015/03/19/Restarting-org-babel-sessions-in-org-mode-more-effectively/

(defun src-block-in-session-p (&optional name)
  "Return if src-block is in a session of NAME.
NAME may be nil for unnamed sessions."
  (let* ((info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params))))

    (cond
     ;; unnamed session, both name and session are nil
     ((and (null session)
           (null name))
      t)
     ;; Matching name and session
     ((and
       (stringp name)
       (stringp session)
       (string= name session))
      t)
     ;; no match
     (t nil))))

(defun org-babel-restart-session-to-point (&optional arg)
  "Restart session up to the src-block in the current point.
Goes to beginning of buffer and executes each code block with
`org-babel-execute-src-block' that has the same language and
session as the current block. ARG has same meaning as in
`org-babel-execute-src-block'."
  (interactive "P")
  (unless (org-in-src-block-p)
    (error "You must be in a src-block to run this command"))
  (let* ((current-point (point-marker))
         (info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        ;; goto start of block
        (goto-char (match-beginning 0))
        (let* ((this-info (org-babel-get-src-block-info))
               (this-lang (nth 0 this-info))
               (this-params (nth 2 this-info))
               (this-session (cdr (assoc :session this-params))))
            (when
                (and
                 (< (point) (marker-position current-point))
                 (string= lang this-lang)
                 (src-block-in-session-p session))
              (org-babel-execute-src-block arg)))
        ;; move forward so we can find the next block
        (forward-line)))))

(defun org-babel-kill-session ()
  "Kill session for current code block."
  (interactive)
  (unless (org-in-src-block-p)
    (error "You must be in a src-block to run this command"))
  (save-window-excursion
    (org-babel-switch-to-session)
    (kill-buffer)))

(defun org-babel-kill-and-restart-session-to-point ()
  "Kill session for current code block."
  (interactive)
  (org-babel-kill-session)
  (org-babel-restart-session-to-point)
)

(defun org-babel-remove-result-buffer ()
  "Remove results from every code block in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-babel-src-block-regexp nil t)
      (org-babel-remove-result))))

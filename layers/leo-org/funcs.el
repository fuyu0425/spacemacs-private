(defun leo/org-insert-clipboard ()
  (interactive)
  (let* (
         (image-file (concat
                      (org-download--dir)
                      "/"
                      (format-time-string "%Y%m%d_%H%M%S_.png")))
         (exit-status
          (call-process "pngpaste" nil nil nil
                        image-file)))
    (org-insert-link nil (concat "file:" image-file) "")
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

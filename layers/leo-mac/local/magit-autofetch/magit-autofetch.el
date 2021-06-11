;;; magit-autofetch.el --- magit auto fetch repos -*- lexical-binding: t; -*-

(require 'magit)
(require 'alert)

(defgroup magit-autofetch nil
  "Automatically fetch git repositories."
  :group 'tools)


;;;###autoload
(define-minor-mode magit-autofetch-mode
  "Fetch git repositories periodically."
  :init-value nil
  :group 'magit-autofetch
  :require 'magit-autofetch
  :global t
  :lighter " git-af"
  (if magit-autofetch-mode
      (magit-autofetch-setup)
      (magit-autofetch-stop)))

(defcustom magit-autofetch-notify t
  "Whether to notify in case of new commits."
  :package-version '(magit-autofetch . "0.1.0")
  :group 'magit-autofetch
  :type 'boolean)

(defcustom magit-autofetch-notify-type 'notifier
  "Whether to notify in case of new commits."
  :package-version '(magit-autofetch . "0.1.0")
  :group 'magit-autofetch
  :type (alert-styles-radio-type 'radio))

(defcustom magit-autofetch-initial-delay 10
  "Initial delay in seconds before fetching."
  :package-version '(magit-autofetch . "0.1.0")
  :group 'magit-autofetch
  :type 'integer)

(defcustom magit-autofetch-interval 900
  "Auto-fetch interval in seconds."
  :package-version '(magit-autofetch . "0.1.0")
  :group 'magit-autofetch
  :type 'integer)

(defcustom magit-autofetch-timeout nil
  "Timeout in seconds for git processes or nil to disable."
  :package-version '(magit-autofetch . "0.1.1")
  :group 'magit-autofetch
  :type 'integer)

(defcustom magit-autofetch-ping-host "8.8.8.8"
  "Host to ping on order to check for Internet connectivity or nil to disable."
  :package-version '(magit-autofetch . "0.1.2")
  :group 'magit-autofetch
  :type 'string)

(defcustom magit-autofetch-fetch-args '("--no-progress")
  "Additional arguments for git fetch."
  :package-version '(magit-autofetch . "0.1.2")
  :group 'magit-autofetch
  :type '(repeat string))

(defcustom magit-autofetch-process-filter nil
  "Optional filter for fetch process."
  :package-version '(magit-autofetch . "0.1.2")
  :group 'magit-autofetch
  :type '(choice function (const nil)))

(defcustom magit-autofetch-after-fetch-hook nil
  "Hooks to run after fetching a repository.
Note: runs in the git fetch buffer, so you can use projectile
functions like `projectile-project-root` to determine project
parameters."
  :group 'magit-autofetch
  :type 'hook)

(defcustom magit-autofetch-after-successful-fetch-hook nil
  "Hooks to run after sucessfully fetching a repository.
In contrast to `magit-autofetch-after-fetch-hook`, these
hooks only run when new git objects were fetched.
Note: runs in the git fetch buffer, so you can use projectile
functions like `projectile-project-root` to determine project
parameters."
  :group 'magit-autofetch
  :type 'hook)


(defun magit-autofetch-run ()
  "Fetch all repositories and notify user."
  (if magit-autofetch-ping-host
      (make-process :name "magit-autofetch-ping"
                    :buffer "*magit-autofetch-ping"
                    :command `("ping" "-c" "1" "-W" "3" ,magit-autofetch-ping-host)
                    :sentinel 'magit-autofetch--ping-sentinel
                    :noquery t))
      (magit-autofetch--work))

(defun magit-autofetch--ping-sentinel (process event)
  "Sentinel function for PROCESS to check ping success given EVENT."
  (when (string= "finished\n" event)
    (magit-autofetch--work))
  (let ((buffer (process-buffer process)))
    (when (not (get-buffer-process buffer))
      (delete-process process)
      (kill-buffer buffer))))

;; (defun magit-autofetch-run ()
;;   (let (repos (magit-list-repos)))
;;   )

;; (defun magit-autofetch--work ()
;;   "Worker function to fetch all repositories."
;;   (interactive)
;;   (let ((repos (magit-list-repos)))
;;     (dolist (repo repos)
;;         (setq default-directory repo)
;;         ;; (print (format "%s" repo))
;;         (magit-fetch-from-upstream (magit-get-current-remote t) (magit-fetch-arguments))
;;        )))


(defvar project-list nil
  "project remained to fetch")
(defun magit-autofetch--work-project (project)
  "Actual call process"
  (let ((default-directory project))
    (when (and (file-directory-p ".git")
               (car (ignore-errors
                      (process-lines "git" "config" "--get" "remote.origin.url"))))
      (let* ((buffer (generate-new-buffer " *git-fetch"))
             (process
              (apply #'start-process "git-fetch" buffer "git" "fetch" magit-autofetch-fetch-args)))
        ;; (print (format "%s" project))
        (process-put process 'magit-project project)
        (set-process-query-on-exit-flag process nil)
        (set-process-sentinel process #'magit-autofetch-sentinel)
        (when magit-autofetch-timeout
          (add-timeout magit-autofetch-timeout 'magit-autofetch-timeout-handler process))))))

(defun magit-autofetch--work-next-project ()
  "Run the first project in project list"
  (unless (null project-list)
    (let ((project (car project-list)))
      (setq project-list (cdr project-list))
      (magit-autofetch--work-project project))))

(defun magit-autofetch-sentinel (process _)
  "Handle the state of PROCESS."
  (unless (process-live-p process)
    (let ((buffer (process-buffer process))
          (default-directory (process-get process 'magit-project)))
      (with-current-buffer buffer
        (run-hooks 'magit-autofetch-after-fetch-hook)
        (when (> (buffer-size) 0)
          (run-hooks 'magit-autofetch-after-successful-fetch-hook)
          (when magit-autofetch-notify
            (let*  (
                    (project-path (process-get process 'magit-project))
                    (project-name (file-name-nondirectory (directory-file-name project-path)))
                    (title "Magit Autofetch")
                    (msg (format "new commit from %s %s" project-name (buffer-string)))
                    )
            (if magit-autofetch-notify-type
                (let ((alert-default-style magit-autofetch-notify-type))
                  (alert msg :title title))
                (alert msg :title title))
            ))))
      (delete-process process)
      (kill-buffer buffer)))
  (magit-autofetch--work-next-project)
  )

(defun magit-autofetch--work ()
  "Worker function to fetch all repositories."
  (interactive)
  (let ((projects (magit-list-repos)))
    (setq project-list projects)
    (magit-autofetch--work-next-project)))

(defun magit-autofetch-timeout-handler (process)
  "Timeout handler to kill slow or blocked PROCESS."
  (delete-process process))

(defvar magit-autofetch-timer nil
  "Timer object for git fetches.")

(defun magit-autofetch-setup ()
  "Set up timers to periodically fetch repositories."
  (interactive)
  (unless (timerp magit-autofetch-timer)
    (setq magit-autofetch-timer
          (run-with-timer
           magit-autofetch-initial-delay
           magit-autofetch-interval
           'magit-autofetch-run))))

(defun magit-autofetch-stop ()
  "Stop auto fetch timers."
  (interactive)
  (cancel-timer magit-autofetch-timer)
  (setq magit-autofetch-timer nil))


(provide 'magit-autofetch)

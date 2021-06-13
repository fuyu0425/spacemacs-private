;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(let*
  ((last-args (car (cdr command-line-args))))
  (if (equal "-persp-q" last-args)
      (setq persp-auto-resume-time -1
            persp-auto-save-opt 0)))

(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . dark)) ;; or dark - depending on your theme

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


(defun when-mac (body)
  (when (eq system-type 'darwin) (funcall body)))

(defun when-linux (body)
  (when (eq system-type 'gnu/linux) (funcall body)))

(defun if-mac-else-linux (body_mac body_linux)
  (if (eq system-type 'darwin)
      (funcall body_mac)
    (if (eq system-type 'gnux/linux) (funcall body_linux))))

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   `(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     themes-megapack
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      ;; auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      )
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first t)
     (colors
      ;; :variables colors-enable-nyan-cat-progress-bar t
             )
     dash
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     search-engine
     pdf
     ;; leo-pdf
     (latex :variables
            latex-enable-magic t
            latex-enable-folding t)
     bibtex
     (ranger :variables
             ranger-cleanup-on-disable t
             ranger-cleanup-eagerly t
             ranger-show-hidden t)
     ;; languages
     (shell-scripts :variables shell-scripts-backend 'lsp)
     asm
     (lsp :variables
          lsp-keep-workspace-alive nil
          lsp-idle-delay 1.0
          lsp-log-io nil
          lsp-enable-folding nil
          lsp-enable-text-document-color nil
          lsp-enable-symbol-highlighting nil
          lsp-enable-links nil
          lsp-headerline-breadcrumb-enable nil
          lsp-signature-auto-activate nil
          lsp-before-save-edits nil
          lsp-completion-enable-additional-text-edit nil
          lsp-enable-on-type-formatting nil
          lsp-eldoc-enable-hover nil
          lsp-enable-file-watchers nil
          lsp-ui-doc-enable nil
          ;; lsp-ui-imenu-enable nil
          ;; rust
          lsp-rust-server 'rust-analyzer
          )
     dap
     php
     go
     json
     ;; haskell
     java
     javascript
     ;; scala
     ocaml
     (c-c++ :variables
            c-c++-backend ',(cond ((eq system-type 'darwin) 'lsp-clangd)
                                  ((eq system-type 'gnu/linux) 'lsp-clangd)) ; lsp-ccls alternative
            c-c++-enable-clang-format-on-save nil)
     html
     sml
     emacs-lisp
     rust
     (python :variables
             python-backend 'lsp
             python-lsp-server 'mspyls
             python-formatter 'yapf
             )
     (ipython-notebook :variables ein-backend 'jupyter)
     (yaml :variables yaml-enable-lsp t)
     fsharp
     lua
     vimscript
     ruby
     ;; tools
     prodigy ; services
     (elfeed :variables rmh-elfeed-org-files '(,(expand-file-name "private/elfeed.org" dotspacemacs-directory)))
     ,(if (eq system-type 'darwin) 'gnus)
     ,(if (eq system-type 'darwin)
          '(mu4e :variables
                 mu4e-mu-binary "/usr/local/Cellar/mu/1.4.15/bin/mu"
                 user-mail-address "a0919610611@gmail.com"
                 mu4e-installation-path "/usr/local/Cellar/mu/1.4.15/share/emacs/site-lisp"
                 mu4e-maildir "~/.mail"
                 mu4e-drafts-folder "/gmail/[Gmail]/Drafts"
                 mu4e-sent-folder "/gmail/[Gmail]/Sent Mail"
                 mu4e-trash-folder "/gmail/[Gmail]/Trash"
                 mu4e-attachment-dir "~/Downloads"
                 mu4e-sent-messages-behavior 'delete
                 ;; https://www.reddit.com/r/emacs/comments/8q84dl/tip_how_to_easily_manage_your_emails_with_mu4e/
                 mu4e-change-filenames-when-moving t
                 mu4e-get-mail-command "mbsync -a"
                 mu4e-update-interval 900
                 mu4e-maildir-shortcuts '(("/gmail/INBOX" . ?g)
                                          ("/gatech/INBOX" . ?G))
                 mu4e-bookmarks
                 `(("flag:unread AND NOT flag:trashed AND NOT (\"maildir:/gmail/[Gmail]/Sent Mail\" OR maildir:/gmail/[Gmail]/Drafts OR maildir:/gatech/Sent Items OR maildir:/gatech/Drafts)" "Unread messages" ?u)
                   ("date:today..now AND NOT (\"maildir:/gmail/[Gmail]/Sent Mail\" OR maildir:/gmail/[Gmail]/Drafts OR \"maildir:/gatech/Sent Items\" OR maildir:/gatech/Drafts)" "Today's messages" ?t)
                   ("date:7d..now AND NOT (maildir:/gmail/[Gmail]/Sent Mail OR maildir:/gmail/[Gmail]/Drafts OR maildir:/gatech/Sent Items OR maildir:/gatech/Drafts)" "Last 7 days" ?w)
                   ("mime:image/*" "Messages with images" ?p)
                   (,(mapconcat 'identity
                                (mapcar
                                 (lambda (maildir)
                                   (concat "maildir:" (car maildir)))
                                 mu4e-maildir-shortcuts) " OR ")
                    "All inboxes" ?i)
                   )
                 mu4e-enable-mode-line t
                 mu4e-enable-async-operations t
                 mu4e-use-maildirs-extension t
                 mu4e-maildirs-extension-hide-empty-maildirs t
                 ;; alert
                 mu4e-enable-notifications t
                 mu4e-alert-email-notification-types '(subjects)

                 ))
     ;; smex
     pandoc
     helpful
     (cmake
      ;; :variables cmake-enable-cmake-ide-support t
      )

     docker
     git
     (version-control :variables
                      version-control-diff-tool 'diff-hl)

     ;; helm
     ivy
     (markdown :variables markdown-live-preview-engine 'vmd)
     multiple-cursors
     deft
     (shell :variables
            shell-default-shell 'vterm
            shell-default-height 50
            shell-default-position 'bottom)
     (treemacs :variables
               treemacs-use-scope-type 'Perspectives)
     version-control
     copy-as-format
     ;; vagrant
     xclipboard
     ;; my-layers
     leo-common
     ,(cond ((eq system-type 'darwin) 'leo-mac)
            ((eq system-type 'gnu/linux) 'leo-linux))
     ;; cryptoline
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      company-posframe
                                      ivy-posframe
                                      all-the-icons-ibuffer
                                      all-the-icons-ivy-rich
                                      burly
                                      bufler
                                      ;; helm-bufler
                                      (mu4e-dashboard :location (recipe :fetcher github :repo "rougier/mu4e-dashboard"))
                                      (elfeed-dashboard :location (recipe :fetcher github :repo "Manoj321/elfeed-dashboard"))
                                      devdocs-browser
                                      mu4e-views
                                      (explain-pause-mode :location (recipe :fetcher github :repo "lastquestion/explain-pause-mode"))
                                      org-fragtog
                                      xclip
                                      all-the-icons
                                      all-the-icons-dired
                                      magit-todos
                                      z3-mode
                                      org-pdftools
                                      ;; org-noter
                                      exec-path-from-shell
                                      ssh-agency
                                      keychain-environment
                                      magit-delta
                                      paren-face
                                      ;; visual-fill-column
                                      jetbrains-darcula-theme
                                      xenops
                                      s ;; s.el
                                      org-roam-bibtex
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    forge
                                    evil-magit
                                    importmagic
                                    )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.

  ;; useful when elpa is temporary down
  ;; (setq configuration-layer-elpa-archives
  ;;     '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
  ;;          ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
  ;;          ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 10

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(200000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         monokai
                         moe-dark
                         doom-monokai-pro
                         dracula
                         solarized-dark
                         spacemacs-dark
                         spacemacs-light
                         jetbrains-darcula)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Monaco"
                               :size 16.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t ; for use evil-jump-forward (C-i)

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Home"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 5

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'original

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols nil

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers '(:enabled-for-modes
                               ;; coq-mode
                               mu4e-main-mode
                               prog-mode
                               text-mode
                               treemacs-mode
                               isar-mode
                               :visual t
                               :relative t)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format nil

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq monokai-highlight      "#535246"
        monokai-highlight-line "#353630")

  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq-default git-magit-status-fullscreen t)
  (setq winum-scope 'frame-local)
  ;; (setq helm-follow-mode-persistent t)
  )


(defun my-reset-frame-size (&optional frame)
  (interactive)
  (when (display-graphic-p)
    (setq
     ;; display-width (x-display-pixel-width)
     ;; display-height (x-display-pixel-height)
     display-width (nth 2 (frame-monitor-geometry))
     display-height (nth 3 (frame-monitor-geometry))
     display-x (nth 0 (frame-monitor-geometry))
     display-y (nth 1 (frame-monitor-geometry))
     )
    (setq
     reset-frame-width (round (* display-width 0.9))
     reset-frame-height (round (* display-height 0.9) )
     reset-frame-width-margin (+ display-x (round (* display-width 0.05)))
     reset-frame-height-margin (+ display-y (round (* display-height 0.05)))
     )
    (setq frame-resize-pixelwise t)
    (when frame (select-frame frame))
    ;; (when window-system
    (set-frame-size (selected-frame) reset-frame-width reset-frame-height t)
    (set-frame-position (selected-frame) reset-frame-width-margin reset-frame-height-margin)
    (setq neo-theme 'icons)
    ;; )
    ;; (custom-set-faces (if (not (display-graphic-p)) '(default ((t (:background "nil"))))))
    ))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
                                        ;(spacemacs/dump-modes '(coq-mode LaTeX-mode))
       )

(defun leo/configure-git ()
  ;; (magit-todos-mode)
  ;; (magit-delta-mode)
  (setq magit-refresh-status-buffer nil)
  (setq
   magit-blame-styles '((margin
                         (margin-format " %s%f" " %C %a" " %H")
                         (margin-width . 42)
                         (margin-face . magit-blame-margin)
                         (margin-body-face magit-blame-dimmed))
                        (headings
                         (heading-format . "%-20a %C %s "))
                        (highlight
                         (highlight-face . magit-blame-highlight))
                        (lines
                         (show-lines . t)
                         (show-message . t)))
   magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
                           ("Branch" 25 magit-repolist-column-branch nil)
                           ("Version" 25 magit-repolist-column-version nil)
                           ("B<U" 3 magit-repolist-column-unpulled-from-upstream
                            ((:right-align t)
                             (:help-echo "Upstream changes not in branch")))
                           ("B>U" 3 magit-repolist-column-unpushed-to-upstream
                            ((:right-align t)
                             (:help-echo "Local changes not in upstream")))
                           ("Path" 99 magit-repolist-column-path nil))
   magit-diff-use-overlays nil
   magit-save-repository-buffers 'dontask
   magit-revision-show-gravatars nil
   )
  (when-mac 'magit-autofetch-mode)
  )

(defun leo/configure-evil ()
  (when (not (display-graphic-p))
    (setq evil-toggle-key "C-`"))
  (setq-default
   evil-want-Y-yank-to-eol nil
   evil-ex-visual-char-range t
   evil-escape-key-sequence "jk"
   evil-escape-unordered-key-sequence "true"
   evil-escape-delay 0.2)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop))

(defun leo/configure-persp ()
  (setq layouts-enable-autosave t
        layouts-autosave-delay 600)
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  ;; (setq persp-autokill-buffer-on-remove nil)
  )

(defun leo/configure-projectile ()
  (setq projectile-file-exists-local-cache-expire (* 5 60))
  )


;; hack becasue I ovewrite C-/ for comment
(define-globalized-minor-mode my-global-undo-tree-mode
  undo-tree-mode (lambda () (undo-tree-mode 1)))


(defun my/org-ref-notes-function (candidates)
  (let ((key (helm-marked-candidates)))
    (funcall org-ref-notes-function (car key))))
(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (my-global-undo-tree-mode 1)
  (evil-set-undo-system 'undo-tree)
  ;; (setq exec-path-from-shell-arguments '("-i"))
  ;; (setq shell-file-name "/opt/local/bin/zsh")
  ;; (exec-path-from-shell-initialize)
  ;; (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  ;; (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  (my-reset-frame-size)
  (add-hook 'after-make-frame-functions 'my-reset-frame-size t)
  ;; (defvaralias 'helm-c-yas-space-match-any-greedy 'helm-yas-space-match-any-greedy "Temporary alias for Emacs27")
  (setq auto-save-interval 60)
  (setq tags-add-tables nil)
  (setq inhibit-compacting-font-caches t)
  (setq system-time-locale "C")
  (leo/configure-git)
  (leo/configure-evil)
  (leo/configure-persp)
  (leo/configure-projectile)
  ;; turn of symlinks ask
  (setq vc-follow-symlinks t)

  ;; (setq garbage-collection-messages t)
  (setq-default fill-column 80)
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (spacemacs/toggle-highlight-long-lines-globally-off)
  (spacemacs/toggle-mode-line-battery-off)
  (spacemacs/toggle-indent-guide-globally-on)
  (spacemacs-modeline/init-spaceline)
  (with-eval-after-load 'spaceline-config
    (spacemacs/toggle-mode-line-battery-on)
    (spacemacs/toggle-display-time-on)
    (spaceline-toggle-minor-modes-off)
    )
  ;; (setq shell-file-name "/bin/bash")
  (setq projectile-enable-caching t)
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (menu-bar-mode 1)
  (setq scroll-preserve-screen-position nil)
  (setq overlay-arrow-string "")
  (defadvice switch-to-buffer (before save-buffer-now activate)
    (when buffer-file-name (save-buffer)))
  (defadvice other-window (before other-window-now activate)
    (when buffer-file-name (save-buffer)))
  (defadvice winum-select-window-by-number (before other-window-now activate)
    (when buffer-file-name (save-buffer)))
  ;; (setq my-helm-swoop-previous-key (kbd "C-s"))
  ;; (setq my-helm-swoop-next-key (kbd "C-a"))
  ;; (eval-after-load "helm"
  ;;   (lambda ()
  ;;     (define-key helm-map my-helm-swoop-previous-key 'previous-history-element)
  ;;     (define-key helm-map my-helm-swoop-next-key 'next-history-element)
  ;;     ))
  (setq minibuffer-previous-key (kbd "C-s"))
  (setq minibuffer-next-key (kbd "C-a"))
  (eval-after-load "ivy"
    (lambda ()
      (define-key ivy-minibuffer-map minibuffer-previous-key 'previous-history-element)
      (define-key ivy-minibuffer-map minibuffer-next-key 'next-history-element)
      (define-key ivy-mode-map minibuffer-previous-key 'previous-history-element)
      (define-key ivy-mode-map minibuffer-next-key 'next-history-element)
      ))
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first"))
  ;; (defvar company-etags-modes '(prog-mode c-mode objc-mode c++-mode java-mode
  ;;                                         jde-mode pascal-mode perl-mode python-mode
  ;;                                         coq-mode
  ;;                                         ))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-source-correlate-mode t)
              (setq TeX-source-correlate-start-server t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
                    )
              (add-hook 'TeX-after-compilation-finished-functions
                        #'TeX-revert-document-buffer)
              ))
  (setq pdf-sync-backward-display-action t)
  (setq pdf-sync-forward-display-action t)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (setq google-translate-default-target-language "zh-TW")
  ;; (add-hook 'focus-out-hook (lambda ()
  ;;                             (save-some-buffers t)
  ;;                             (garbage-collect)
  ;;                             ))

  ;; Some Keyborad Mapping
  ;; Avy jump
  (define-key evil-normal-state-map (kbd "M-l") #'avy-goto-line)
  (define-key evil-normal-state-map (kbd "M-s") #'avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "M-w") #'avy-goto-word-1)
  (define-key evil-motion-state-map (kbd "M-w") #'avy-goto-word-1)
  ;; evil-nerd-commentator
  (define-key evil-normal-state-map (kbd "C-/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-hybrid-state-map (kbd "C-/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-hybrid-state-map (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  (evil-ex-define-cmd "q[uit]" 'kill-buffer-and-window)
  (evil-ex-define-cmd "wq" (lambda ()
                             (interactive)
                             (save-buffer)
                             (kill-buffer-and-window)
                             ))

  ;; (add-to-list 'spacemacs-indent-sensitive-modes 'cryptoline-mode)
  (add-to-list 'auto-mode-alist '("\\.gas\\'" . asm-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))

  (setq python-shell-completion-native-enable nil)
  (setq kill-buffer-query-functions nil)
  ;; (add-to-list 'load-path "~/source/emacs-mode/simp-isar-mode")
  ;; (require 'isar-mode)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  ;; (use-package visual-fill-column
  ;;   :config
  ;;   (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  ;;   (setq-default split-window-preferred-function 'visual-fill-column-split-window-sensibly)
  ;;   (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))
  (setq reftex-default-bibliography '("~/org/bibtex/library.bib"))
  (setq org-ref-default-bibliography '("~/org/bibtex/library.bib")
        org-ref-pdf-directory "~/org/papers/"
        org-ref-notes-function 'org-ref-notes-function-many-files)
  (setq bibtex-completion-bibliography '("~/org/bibtex/library.bib")
        bibtex-completion-library-path '("~/org/papers")
        bibtex-completion-notes-path "~/org/paper_notes")
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-find-additional-pdfs t)

  (defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
    (let ((bibtex-completion-pdf-open-function
           (lambda (fpath) (start-process "open" "*open*" "open" fpath))))
      (bibtex-completion-open-pdf keys fallback-action)))
  
  (defun bibtex-completion-open-pdf-emacs (keys &optional fallback-action)
    (let ((bibtex-completion-pdf-open-function
           (lambda (fpath) (find-file fpath))))
      (bibtex-completion-open-pdf keys fallback-action)))


  (require 'ivy-bibtex)
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath) (start-process "open" "*open*" "open" fpath)))
  ;; (ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)
  (ivy-bibtex-ivify-action bibtex-completion-open-pdf-emacs ivy-bibtex-open-pdf-emacs)

  ;; (ivy-add-actions
  ;;  'ivy-bibtex
  ;;  '(("P" ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)")))

  (ivy-add-actions
   'ivy-bibtex
   '(("P" ivy-bibtex-open-pdf-emacs "Open PDF in Emacs")))

  (with-eval-after-load 'ivy-bibtex
    (spacemacs/set-leader-keys
      "ab" 'ivy-bibtex))
  (setq bibtex-completion-notes-template-multiple-files
        (format "#+TITLE: ${title}\n#+AUTHOR: ${author-or-editor}\n#+KEY: ${=key=}\n#+KEYWORDS: ${keywords}\n#+PROPERTIES: NOTER_DOCUMENT ${file}\n#+YEAR: ${year}\n\ncite:${=key=}\n\n* TODO Summary\n\n* TODO Novelty\n\n* TODO Questions\n\n* Abstract and Introduction\n\n* Related Work\n\n* System Design\n\n* Evaluation"))
  ;; (setq org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename)
  (setq terminal-here-terminal-command (list "open" "-a" "iTerm" "."))
  (setq elisp-dir (expand-file-name "elisp" dotspacemacs-directory))
  (add-to-list 'load-path elisp-dir)
  (require 'mu4e-thread-folding)
  (setq mu4e-thread-folding-default-view 'folded)

  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                                 :shortname ""
                                 :function (lambda (msg) "  "))))
  (setq mu4e-headers-fields '((:empty         .    2)
                              (:human-date    .   12)
                              (:flags         .    6)
                              (:mailing-list  .   10)
                              (:from          .   22)
                              (:subject       .   nil)))
  ;; persistent undo
  ;; (setq undo-tree-auto-save-history t
  ;;       undo-tree-history-directory-alist
  ;;       `(("." . ,(concat spacemacs-cache-directory "undo"))))
  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))
  (setq transient-enable-popup-navigation t)
  (with-eval-after-load 'transient
    (transient-bind-q-to-quit))

  (with-eval-after-load 'org
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted)
    (setq org-latex-minted-options
          '(("frame" "lines") ("linenos=true")))
    (setq org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    )
  (define-key spacemacs-latex-mode-map-root-map (kbd "<f5>") 'latex/build)
  ;; (evil-global-set-key 'normal "S" #'flyspell-correct-at-point)
  (setq
   lsp-diagnostics-provider :auto
   company-idle-delay 1.0
   company-minimum-prefix-length 3)
  ;; refresh keychain
  (run-with-timer
   300
   3600
   'keychain-refresh-environment)
  (with-eval-after-load 'treemacs
    (treemacs-follow-mode -1)
    (treemacs-filewatch-mode -1))
  ;; make xclip a default behavior => make evil-yank use xclip/xsel
  (xclip-mode 1)
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (spacemacs/set-leader-keys
    "atl" 'lsp)
  ;; (define-key mu4e-headers-mode-map (kbd "R"))
  (use-package mu4e-views
  :after mu4e
  :defer nil
  :bind (:map mu4e-headers-mode-map
	    ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
	    ("M-j" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	    ("M-k" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
        ("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
        ("i" . mu4e-views-mu4e-view-as-nonblocked-html) ;; show currently selected email with all remote content
	    )
  :config
  ;; (setq mu4e-views-completion-method 'helm) ;; use ivy for completion
  (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
  (setq mu4e-views-default-view-method "html") ;; make xwidgets default
  (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ;; when pressing n and p stay in the current window
  (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view
  ;; (explain-pause-mode 1)
  (defun mu4e-views-mu4e-view-in-browser ()
    "Wraps the `mu4e-view-action' function.
Passes on the message stored in `mu4e-views--current-mu4e-message'."
    (interactive)
    (mu4e-action-view-in-browser mu4e-views--current-mu4e-message))

  (define-key mu4e-views-view-actions-mode-map (kbd "v") 'mu4e-views-mu4e-view-in-browser)
  (define-key mu4e-view-mode-map (kbd "v") 'mu4e-views-mu4e-view-in-browser)
  ;; (define-key mu4e-headers-mode-map (kbd "v") 'mu4e-action-view-in-browser)
  (setq gnus-keep-backlog t
        gnus-asynchronous t
        gnus-use-cache  t
        gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil)
  (setq gnus-select-method '(nnml ""))
  (setq gnus-secondary-select-methods '((nntp "news.gwene.org")))
  (remove-hook 'gnus-mark-article-hook
               'gnus-summary-mark-read-and-unread-as-read)
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (with-eval-after-load 'elfeed
    (run-with-timer 0 (* 60 60 4) 'elfeed-update))
  (spacemacs|define-custom-layout "@elfeed"
    :binding "f"
    :body
    (call-interactively 'elfeed))
  ;; https://github.com/zamansky/using-emacs/blob/master/myinit.org
  (require 'elfeed)
  (setq elfeed-search-remain-on-entry t)
  (defun elfeed-mark-all-as-read ()
	  (interactive)
	  (mark-whole-buffer)
	  (elfeed-search-untag-all-unread))


  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun bjm/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;;write to disk when quiting
  (defun bjm/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))

  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))

  (with-eval-after-load 'elfeed
                       (define-key elfeed-search-mode-map (kbd "J") 'leo/make-and-run-elfeed-hydra))

  (defun z/hasCap (s) ""
	       (let ((case-fold-search nil))
	         (string-match-p "[[:upper:]]" s)
	         ))

  (defun z/get-hydra-option-key (s)
    "returns single upper case letter (converted to lower) or first"
    (interactive)
    (let ( (loc (z/hasCap s)))
      (if loc
	        (downcase (substring s loc (+ loc 1)))
	      (substring s 0 1)
        )))

  (defun leo/make-elfeed-cats (tags)
    "Returns a list of lists. Each one is line for the hydra configuratio in the form
       (c function hint)"
    (interactive)
    (remove nil (mapcar (lambda (tag)
	            (let* (
		                 (tagstring (symbol-name tag))
		                 (c (z/get-hydra-option-key tagstring))
		                 )
                (if (not (string= tagstring "star"))
		            (list c (append '(elfeed-search-set-filter) (list (format "@6-months-ago +%s" tagstring) ))tagstring)
                (list)
                )))
	          tags)))

  (defun elfeed-filter-toggle-unread ()
    (interactive)
    (let* (
           (taglist (s-split " " elfeed-search-filter))
           (in (member "+unread" taglist))
           )
    (if in
        (elfeed-search-set-filter (s-join " " (remove "+unread" taglist)))
      (elfeed-search-set-filter (concat elfeed-search-filter " +unread"))
     )
    ))

  (defmacro leo/make-elfeed-hydra ()
    `(defhydra leo/hydra-elfeed ()
       "filter"
       ,@(leo/make-elfeed-cats (elfeed-db-get-all-tags))
       ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
       ("M" elfeed-toggle-star "Mark")
       ("R" elfeed-filter-toggle-unread "(un)read")
       ("A" (elfeed-search-set-filter "@6-months-ago") "All")
       ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
       ("Q" bjm/elfeed-save-db-and-bury "Quit Elfeed" :color blue)
       ("q" nil "quit" :color blue)
       ))
  (defvar elfeed-is-unread t)
  (defun leo/elfeed-toggle-unread ()
    (interactive)
    (setq elfeed-is-unread (not elfeed-is-unread))
    (elfeed-filter-toggle-unread))
  (defun leo/make-and-run-elfeed-hydra ()
    ""
    (interactive)
    (leo/make-elfeed-hydra)
    (leo/hydra-elfeed/body))

  (defun leo/elfeed-construct-filter (tag)
    (interactive "MTag Name: ")
    (let* ((filter-string (pcase tag
                           ("Today" (format "@1-day-ago"))
                           ("All" (format "@6-months-ago"))
                           (_ (format "@6-months-ago +%s" tag)))))
          (if elfeed-is-unread (concat filter-string " +unread") filter-string)))

  (defun leo/elfeed-search-filter-ivy ()
    "Set elfeed filter by ivy"
    (interactive)
    (ivy-read "Tag: "
              (append '("Today" "All") (elfeed-db-get-all-tags))
              :action (lambda (tag)
                        (let* ((filter-string (leo/elfeed-construct-filter tag)))
                          (elfeed-search-set-filter filter-string)))
              :history 'leo/elfeed-search-filter-ivy-history
              :require-match t
              :preselect t
              :caller 'leo/elfeed-search-filter-ivy
              ))

  (defun elfeed-tag-selection-as (mytag)
    "Returns a function that tags an elfeed entry or selection as
MYTAG"
    (lambda ()
      "Toggle a tag on an Elfeed search selection"
      (interactive)
      (elfeed-search-toggle-all mytag)))

  (define-key elfeed-search-mode-map "l" (elfeed-tag-selection-as 'readlater))
  (define-key elfeed-search-mode-map "d" (elfeed-tag-selection-as 'junk))
  (define-key elfeed-search-mode-map "*" (elfeed-tag-selection-as 'star))


  (with-eval-after-load 'mu4e-alert
    ;; Enable Desktop notifications
    (mu4e-alert-set-default-style 'notifier))
  (prodigy-define-tag
    :name 'email
    :ready-message "Checking Email using IMAP IDLE. Ctrl-C to shutdown.")
  (prodigy-define-service
   :name "imapnotify-gmail"
   :command "goimapnotify"
   :args (list "-conf" (expand-file-name "goimapnotify/gmail.conf" (getenv "HOME")))
   :tags '(email)
   :kill-signal 'sigkill)

  (prodigy-define-service
    :name "imapnotify-outlook"
    :command "imapnotify"
    :args (list "-c" (expand-file-name "imapnotify/outlook.js" (getenv "HOME")))
    :tags '(email)
    :kill-signal 'sigkill)
  (defun leo/start-prodigy ()
    (prodigy-start-service  (prodigy-find-service "imapnotify-gmail"))
    (prodigy-start-service  (prodigy-find-service "imapnotify-outlook")))
  (run-with-timer 0 (* 300) 'leo/start-prodigy)
  (leo/start-prodigy)

  (defun fetch-access-token ()
     (with-temp-buffer
	     (call-process "/Users/fuyu0425/.asdf/shims/python" nil t nil "/Users/fuyu0425/bin/oauth2ms" "--encode-xoauth2")
	(buffer-string)))

   ;; Add new authentication method for xoauth2
   (cl-defmethod smtpmail-try-auth-method
     (process (_mech (eql xoauth2)) user password)
     (let* ((access-token (fetch-access-token)))
	(smtpmail-command-or-throw
	 process
	 (concat "AUTH XOAUTH2 " access-token)
	 235)))

   ;;; Register the method
   ;; (with-eval-after-load 'smtpmail
   ;;   (add-to-list 'smtpmail-auth-supported 'xoauth2))

  (setq  smtpmail-queue-dir "~/.mail/queue/cur")
  (setq mu4e-contexts
    `( ,(make-mu4e-context
    :name "gmail"
    :enter-func (lambda () (mu4e-message "Switch to the Gmail context"))
    ;; leave-func not defined
    :match-func (lambda (msg)
      (when msg
        (mu4e-message-contact-field-matches msg
          :to "a0919610611@gmail.com")))
    :vars '((user-mail-address      . "a0919610611@gmail.com"  )
            (user-full-name     . "Yu-Fu Fu" )
            (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
            (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
            (mu4e-trash-folder . "/gmail/[Gmail]/Trash")
            (smtpmail-default-smtp-server . "smtp.gmail.com")
            (smtpmail-smtp-user . nil)
            (smtpmail-local-domain . "localhost")
            (smtpmail-smtp-server . "smtp.gmail.com")
            (smtpmail-smtp-service . 587)
            (message-send-mail-function . async-smtpmail-send-it)
            (send-mail-function . async-smtpmail-send-it)
            (smtpmail-debug-info . t)
            (smtpmail-debug-verb . t)
            (starttls-use-gnutls . t)
            (starttls-gnutls-program . "gnutls-cli")
            (smtpmail-stream-type . starttls)
            (smtpmail-auth-supported . (cram-md5 plain login))
            (mu4e-compose-signature . "Best regards\nYu-Fu, Fu (傅裕夫)")
            ))
       ,(make-mu4e-context
         :name "outlook"
         :enter-func (lambda () (mu4e-message "Switch to the Gatech context"))
         ;; leave-fun not defined
         :match-func (lambda (msg)
                       (when msg
                         (mu4e-message-contact-field-matches msg :to "yufu@gatech.edu")))
         :vars '((user-mail-address      . "yufu@gatech.edu")
                 (user-full-name     . "Yu-Fu Fu")
                 (smtpmail-smtp-user . "yfu330@gatech.edu")
                 (smtpmail-local-domain . "gatech.edu")
                 (mu4e-drafts-folder . "/gatech/Drafts")
                 (mu4e-sent-folder . "/gatech/Sent Items")
                 (mu4e-trash-folder . "/gatech/Deleted Items")
                 (smtpmail-smtp-server . "smtp.office365.com")
                 (smtpmail-smtp-service . 587)
                 (smtpmail-stream-type . starttls)
                 (message-send-mail-function . smtpmail-send-it)
                 (send-mail-function . smtpmail-send-it)
                 (smtpmail-auth-supported . (xoauth2))
                 (mu4e-compose-signature . "Best regards\nYu-Fu, Fu")
                 ))))
  (setq mu4e-org-contacts-file  "~/org/contact.org")
  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (setq leo/emacs-mode-list '(mu4e-headers-mode
                         mu4e-view-mode
                         eww-mode
                         xwidget-webkit-mode
                         elfeed-show-mode
                         ))
  (cl-loop for m in leo/emacs-mode-list
           do (add-to-list 'evil-emacs-state-modes m)
              (evil-set-initial-state m 'emacs))
  (with-eval-after-load 'eww
    (add-to-list 'evil-emacs-state-modes 'eww-mode)
    (evil-set-initial-state 'eww-mode 'emacs))
  (with-eval-after-load 'elfeed
    (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
    (evil-set-initial-state 'elfeed-show-mode 'emacs))
  (with-eval-after-load 'bufler
    (add-to-list 'evil-emacs-state-modes 'bufler-list-mode)
    (evil-set-initial-state 'bufler-list-mode 'emacs))

  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread maildir:/gmail/INBOX "
         "OR "
         "flag:unread maildir:/gatech/INBOX "
         "OR "
         "flag:unread maildir:/gatech/CS6265 "
         "OR "
         "flag:unread maildir:/gatech/Piazza "
         "AND NOT flag:trashed "
         ))
  (defun revert-mu4e-main-buffer ()
    (mu4e-maildirs-extension-force-update)
    (with-current-buffer " *mu4e-main*" (revert-buffer)))

  (add-hook 'mu4e-index-updated-hook 'revert-mu4e-main-buffer)

  (all-the-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1)
  (all-the-icons-ibuffer-mode 1)
  (setq ivy-posframe-height-alist '((swiper . 20)
                                    (t      . 20)))
  (setq ivy-posframe-width 100
        ivy-posframe-height 20)
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))

  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-display-function-fallback)
          (org-ref-ivy-insert-ref-link          . ivy-display-function-fallback)
          (org-ref-ivy-insert-cite-link          . ivy-display-function-fallback)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-window-center)
          (t               . ivy-posframe-display)))
  (ivy-posframe-mode 1)
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (spacemacs/set-leader-keys-for-major-mode
    'elfeed-search-mode
    "," 'leo/elfeed-search-filter-ivy
    "f" 'leo/elfeed-search-filter-ivy
    "t" 'leo/elfeed-toggle-unread)
  ;; (company-posframe-mode 1)
  (defalias 'dnd-unescape-uri 'dnd--unescape-uri)
  (spacemacs/set-leader-keys-for-major-mode
    'bibtex-mode
    "lb" 'biblio-lookup)
  (require 'org-roam-bibtex)
  (add-hook 'org-roam-mode-hook #'org-roam-bibtex-mode)
  (setq orb-templates
        '(("r" "ref" plain #'org-roam-capture--get-point
           ""
           ;; (file "~/org/template/paper_note.org")
           :file-name "paper_notes/${citekey}"
           :head "#+TITLE: ${title}
#+ROAM_KEY: ${citekey}
${ref}

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:AUTHOR: ${author-or-editor-abbrev}
:YEAR: ${year}
:NOTER_DOCUMENT: ${file}
:END:
"
           :unnarrowed t)))
)

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)

(setq secret-file (expand-file-name "secret.el" dotspacemacs-directory))
(load secret-file 'no-error 'no-message)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  )

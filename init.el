(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(general json-mode evil-collection evil tree-sitter-langs tree-sitter yasnippet treemacs helm flycheck treemacs-projectile org-bullets vterm-toggle company-box company-lsp java-lsp lsp-java lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode magit counsel-projectile projectile doom-themes helpful which-key rainbow-delimiters doom-modeline company ivy-rich vterm counsel ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; My emacs config
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(menu-bar-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; set line number
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline nil)

;; set auto parens
(electric-pair-mode t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/backup/" t)))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; font use
(set-face-attribute 'default nil :font "UbuntuMono Nerd Font" :height 150)

(setq visible-bell 1)

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq use-package-always-ensure t)

;; set seperate backup files
(setq backup-directory-alist '(("." . "~/MyEmacsBackups")))

;; set line number
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable for some mode
(dolist (mode '(org-mode-hook
		        term-mode-hook
                vterm-mode-hook
		        eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; which keyb
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; better help
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1))

;; additional evil keybindings for better navigation
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; general.el for setting keybindings
(use-package general)

(defconst my-leader "SPC")

(general-create-definer vim-leader-key
    :keymaps '(normal visual insert emacs)
    :prefix my-leader
    :non-normal-prefix "C-SPC")

;; check hydra for scaling keybinds

;; set theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; treesitter for themes
(use-package tree-sitter
  :config
  (add-hook 'after-init-hook #'tree-sitter-mode)
  (add-hook 'after-init-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

;; modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (setq doom-modeline-height 15))

(use-package all-the-icons)

;; rainbow paren
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy
  :diminish ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(use-package counsel
  :diminish
  :after ivy
  :config
  (global-unset-key (kbd "C-l"))
  (global-set-key (kbd "C-l f") 'counsel-fzf))

(use-package swiper
  :diminish
  :after ivy)

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; basic company setup
(use-package company
  :hook ((prog-mode) . company-mode)
  :init
  (setq company-minimum-prefix-length 1
        company-backends '(company-capf)
        company-idle-delay 0))

;; yas mode
(use-package yasnippet
  :config
  (yas-global-mode 1))


;; company front-end
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; projectile configuration
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom (projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/IdeaProjects")
    (setq projectile-project-search-path '("~/IdeaProjects")))
  (setq projectile-switch-project-action #'projectile-dired))



;; better integration between counsel and projectile
(use-package counsel-projectile
  :config (counsel-projectile-mode))


;; 
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :config
  (global-set-key (kbd "C-c g") 'magit))


;; lsp mode
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)

  (setq lsp-lens-enable t))


;; syntax checking
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook (lambda () (global-flycheck-mode))))

(add-to-list 'display-buffer-alist (cons (rx string-start (eval flycheck-error-list-buffer) string-end)
                                    '(display-buffer-below-selected . ((window-height . shrink-window-if-larger-than-buffer)
                                                                                  (reusable-frames . t)))))
  
;; if you are helm user
(use-package helm)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

;; java lsp
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind
  (:map lsp-mode-map
	("M-RET" . lsp-execute-code-action))
  :config
  (lsp-enable-which-key-integration t)

  ;; turn off the autoformatting
  (setq lsp-enable-on-type-formatting 0)

  ;; setting the default completion provider for lsp
  (setq lsp-completion-provider :capf)

  ;; performance tweaks
  (setq gc-cons-threshold 1000000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.500))


;; set lsp-java
(use-package lsp-java
  :init
  (setq lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java")
  :config
  ;; (setq lsp-java-configuration-runtimes '[(:name "JavaSE-11"
  ;;                                                :path "/usr/lib/jvm/java-11-openjdk-amd64")
  ;;                                         (:name "JavaSE-8"
  ;;                                                :path "/usr/lib/jvm/java-8-openjdk-amd64"
  ;;                                                :default t)])
  (add-hook 'java-mode-hook #'lsp)
  (setq tab-width 4))

;; set up the terminal
(use-package vterm)

;; json mode
(use-package json-mode)

;; format json
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -c 'import sys,json; data=json.loads(sys.stdin.read()); print json.dumps(data,sort_keys=True,indent=4).decode("unicode_escape").encode("utf8","replace")'" (current-buffer) t)))

(define-key json-mode-map (kbd "C-c C-f") 'beautify-json)


;; org mode configuration
;; better org-mode
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; for running java in org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((java . t)))

;; treemacs
(use-package treemacs)
(use-package treemacs-projectile)

;; basic bindings
(vim-leader-key

  ;; toggle
  "t" '(:ignore t :which-key "Toggles")
  "tt" '(counsel-load-theme :which-key "Theme")

  ;; window keybinds
  "w" '(:ignore t :which-key "Window")
  "wk" '(:ignore t :which-key "Kill Window")
  ;; "wkk" '(kill-buffer-and-window :which-key "Kill current window")

  ;; Buffer Keybindings
  "b" '(:ignore t :which-key "Buffers")

  "bb" '(counsel-switch-buffer :which-key "Buffers")

  ;; kill buffer
  "bk" '(:ignore t :which-key "Kill Buffer")
  "bkk" '(kill-current-buffer :which-key "Current")
  "bkb" '(kill-buffer :which-key "Other")

  ;; File related stuff
  "f" '(:ignore t :which-key "Files")
  "ff" '(counsel-find-file :which-key "Find files")

  ;; Project Commands
  "p" '(:ignore t :which-key "Project")
  "pp" '(counsel-projectile-switch-project :which-key "Switch Project")
  "pf" '(counsel-projectile-find-file :which-key "Find File")
  ;; "pg" (counsel-projectile-rg :which-key "Search String")

  ;; Git commands
  "g" '(:ignore t :which-key "Git")
  "gs" '(magit-status :which-key "Status")

  ;; LSP Keybindings (WIP)
  "l" '(:ignore t :which-key "LSP")
  "lc" '(:ignore t :which-key "Code")
  "lca" '(helm-lsp-code-actions :which-key "Code Actions"))

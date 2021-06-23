;; setup package system
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
 '(package-selected-packages
   '(org-bullets vterm-toggle company-box company-lsp java-lsp lsp-java lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode magit counsel-projectile projectile doom-themes helpful which-key rainbow-delimiters doom-modeline company ivy-rich vterm counsel ivy use-package)))
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

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/backup/" t)))

;; font use

(set-face-attribute 'default nil :font "Ubuntu Mono" :height 150)

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

;; set theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

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

 
;; company mode
(use-package company
  :diminish company-mode
  :hook ((prog-mode) . company-mode)
  :bind
  (:map company-active-map
	("<return>" . nil)
	("<tab>" . smarter-tab-to-complete))
  :custom
  (company-minimum-prefix-lenght 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
    ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  :config
  ;; (unless clangd-p (delete 'company-clang company-backends))
  (global-company-mode 1)
  (add-to-list 'company-backends '(company-capf))
  (defun smarter-tab-to-complete ()
    "Try to `org-cycle', `yas-expand', and `yas-next-field' at current cursor position.

If all failed, try to complete the common part with `company-complete-common'"
    (interactive)
    (when yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (func-list
             (if (equal major-mode 'org-mode) '(org-cycle yas-expand yas-next-field)
               '(yas-expand yas-next-field))))
        (catch 'func-suceed
          (dolist (func func-list)
            (ignore-errors (call-interactively func))
            (unless (and (eq old-point (point))
                         (eq old-tick (buffer-chars-modified-tick)))
              (throw 'func-suceed t)))
          (company-complete-common))))))

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
  :commands (magit-status magit-get-current-branch))


;; lsp mode
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)

  (setq lsp-lens-enable t))

  
;; if you are helm user
(use-package helm)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

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

  ;; performance tweaks
  (setq gc-cons-threshold 1000000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.500))


;; set lsp-java
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp)
  (setq tab-width 4))

;; set up the terminal
(use-package vterm)


;; better org-mode
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(moe-theme general json-mode evil-collection evil tree-sitter-langs tree-sitter yasnippet treemacs helm flycheck treemacs-projectile org-bullets vterm-toggle company-box company-lsp java-lsp lsp-java lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode magit counsel-projectile projectile doom-themes helpful which-key rainbow-delimiters doom-modeline company ivy-rich vterm counsel ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; yas mode
(use-package yasnippet
  :config
  (yas-global-mode 1))

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


;; treemacs
(use-package treemacs)
(use-package treemacs-projectile)

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f7fed1aadf1967523c120c4c82ea48442a51ac65074ba544a5aefc5af490893b" default))
 '(package-selected-packages
   '(highlight-indentation modus-themes evil-surround lsp-ui transpose-frame flycheck tree-sitter-langs vterm centaur-tabs counsel-projectile projectile counsel ivy-rich ivy neotree doom-modeline doom-themes all-the-icons which-key general evil-org evil-collection evil org-bullets yasnippet lsp-java lsp-mode company use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:family "Monaco"))))
 '(mode-line ((t (:height 135))))
 '(mode-line-inactive ((t (:height 135))))
 '(org-ellipsis ((t (:foreground unspecified :height 0.8 :inherit 'shadow))))
 '(org-warning ((t (:underline nil)))))
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

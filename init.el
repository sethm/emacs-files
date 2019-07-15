;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This Emacs initialization file bootstraps all settings from the
;; `configuration.org' file, which is in Literate Programming style.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setq load-path (cons "~/.emacs.d/org-mode/contrib/lisp" load-path))
;(setq load-path (cons "~/.emacs.d/org-mode/lisp" load-path))

(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/org-mode/contrib/lisp")

(org-babel-load-file "~/.emacs.d/configuration.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Anything below has been overridden by customization.
;; DO NOT TOUCH IT.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm flycheck-rust racer cargo rust-mode toml-mode haskell-mode htmlize typescript-mode php-mode web-mode lsp-ui lsp-mode company flycheck paredit yasnippet-snippets yasnippet git-gutter magit graphviz-dot-mode ledger-mode fsm url-http-ntlm tabbar org-bullets sml-modeline afternoon-theme auto-package-update use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

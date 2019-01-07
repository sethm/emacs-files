;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Initialization File
;; Author: Seth Morabito <web@loomcom.com>
;; Last Updated: 10-July-2018
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minimize the UI
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Make the title bar not ugly in OS X, and enable keys I like
(when (eq window-system 'ns)
  (add-to-list 'frameset-filter-alist '(ns-transparent-titlebar . :never))
  (add-to-list 'frameset-filter-alist '(ns-appearance . :never))
  (setq mac-option-modifier 'none
        mac-command-modifier 'meta
        mac-function-modifier 'hyper
        mac-right-option-modifier 'super))

;; Basic offsets
(setq-default c-basic-offset 4)
(c-set-offset 'brace-list-intro '+)

;; Prevent lockfiles, I find they cause mayhem.
(setq create-lockfiles nil)

;; Always display line numbers
(global-display-line-numbers-mode)

;; Show (line,column) in the modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; Turn off the annoying (to me) bell and visible bell.
(setq ring-bell-function 'ignore)

;; Highlight matching parens
(show-paren-mode t)

;; Tramp wants these to be happy.  Memory is practically FREE now anyway
(setq max-lisp-eval-depth 4000)		; default is 400
(setq max-specpdl-size 5000)		; default is 1000

;; Translates ANSI colors in shell.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; I hate trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; I prefer tabs to be expanded into spaces by default
(setq-default indent-tabs-mode nil)

;; Enable upcase-region function (why is this disabled by default??)
(put 'upcase-region 'disabled nil)

;; Show the time and date in the bar
(setq display-time-day-and-date t)

;; Desktop saving
(desktop-save-mode 1)
(setq desktop-dirname "~/.emacs.d/")

;; Always wrap split windows
(setq truncate-partial-width-windows nil)

;; Backup and auto save. I like these to be in a unified location, not
;; scattered to the wind.
(if (not (file-exists-p "~/.emacs.d/backups"))
    (make-directory "~/.emacs.d/backups" t))
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq auto-save-default t)

;; Keep the oldest 2 versions, and the newest 5 versions. Disk space is cheap!
(setq kept-old-versions 2)
(setq kept-new-versions 5)

;; Silently delete old versions, don't interrupt saving and ask if it's OK.
(setq delete-old-versions t)

;; Always auto-revert buffers if they are un-edited, and the file changes
;; on disk.
(global-auto-revert-mode t)

;; I'm kind of a dummy, and I need this :B
(setq confirm-kill-emacs 'yes-or-no-p)

;; In remote X11, my delete key behaves incorrectly
(when (eq window-system 'x)
  (normal-erase-is-backspace-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I load slime from Quicklisp

(setq exec-path (append exec-path '("/usr/local/bin")))

(when (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))

;; Load some packages from local locations

(add-to-list 'load-path "~/.emacs.d/local")
(add-to-list 'load-path "~/.emacs.d/lisp")

;;
;; Emacs built-in package management and the Marmalade repo.
;;

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Ledger Mode
(use-package ledger-mode
  :ensure t)

;; Theme
(use-package doom-themes
  :ensure t)

;; Graphviz
(use-package graphviz-dot-mode
  :ensure t)

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-width 30))

;; magit
(use-package magit
  :ensure t)

;; Haskell
(use-package haskell-mode
  :ensure t
  :defer t)

;; Helm mode
(use-package helm
  :ensure t
  :bind (("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("M-x" . helm-M-x))
  :config
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-candidate-number-list 50
        helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  (helm-mode 1))


;; Cargo mode
(use-package cargo
  :ensure t)

;; Rust mode

(setq exec-path (append exec-path '("~/.cargo/bin")))

(use-package rust-mode
  :ensure t
  :defer t
  :bind (("C-c TAB" . rust-format-buffer))
  ;; Note that the hooks are set up here in an 'init:' block
  ;; intentionally! There is a dependency load order problem
  ;; that prevents these from being 'hook:' calls.
  :init
  (add-hook 'rust-mode-hook #'flycheck-mode)
  :config
  (use-package flycheck
    :ensure t))

(use-package lsp-mode
  :ensure t
  :init
  (require 'lsp-clients)
  (add-hook 'rust-mode-hook 'lsp)
  :config
  (use-package lsp-ui
    :ensure t))

;; CEDET
(use-package cedet
  :ensure t
  :init
  (semantic-mode 1)
  (global-semantic-decoration-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (global-semantic-idle-summary-mode 1)
  (global-semantic-idle-local-symbol-highlight-mode 1)
  (global-semantic-highlight-func-mode 1)
  :bind (:map semantic-mode-map
              ("C-c , >" . semantic-ia-fast-jump)))

;; git gutter
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

;; Typescript mode
(use-package typescript-mode
  :ensure t)

;; Paredit mode
(use-package paredit
  :ensure t
  :defer t
  :init
  (autoload 'enable-paredit-mode "paredit" "Structural editing of Lisp")
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  ;; Paredit key bindings
  ;; --------------------
  ;; Paredit messes with my navigation, so I redefine several
  ;; paredit mode keys. Essentially, this changes C-<left>
  ;; and C-<right> into S-C-<left> and S-C-<right>, on multiple
  ;; platforms.
  (define-key paredit-mode-map (kbd "C-<left>") nil)
  (define-key paredit-mode-map (kbd "C-<right>") nil)
  (define-key paredit-mode-map (kbd "C-S-<left>")
    'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-S-<right>")
    'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (read-kbd-macro "S-M-[ 5 D")
    'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (read-kbd-macro "S-M-[ 5 C")
    'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (read-kbd-macro "M-[ 1 ; 6 d")
    'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (read-kbd-macro "M-[ 1 ; 6 c")
    'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (read-kbd-macro "S-M-[ 1 ; 5 D")
    'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (read-kbd-macro "S-M-[ 1 ; 5 C")
    'paredit-forward-slurp-sexp))

;; yasnipets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (add-to-list 'auto-mode-alist '("~/.emacs.d/snippets"))
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet
  :config (yasnippet-snippets-initialize))

;; htmlize
(use-package htmlize
  :ensure t)

;; mu4e - local, may or may not be installed
(when (and (require 'mu4e nil 'noerror)
           (file-exists-p (expand-file-name "~/.emacs.d/local/mail-and-news.el")))
  (load "mail-and-news.el"))

;; Org mode is essential
(use-package org
  :ensure org-plus-contrib
  :bind (("C-c a" . org-agenda))
  :config
  (require 'org-drill)
  (require 'ox-rss)
  (org-add-link-type
   "youtube"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/" handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format youtube-iframe-format
                     path (or desc "")))
       (latex (format "\href{%s}{%s}"
                      path (or desc "video"))))))
  (setq org-pretty-entities t
        org-ellipsis "▼"))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Org agenda setup varies by machine
(when (file-exists-p (expand-file-name "~/.emacs.d/local/org-agenda-setup.el"))
  (load "org-agenda-setup.el"))

;; I prefer to insert periods after section numbers
;; when exporting org-mode to HTML
(defun my-html-filter-headline-yesdot (text backend info)
  "Ensure dots in headlines."
  (when (org-export-derived-backend-p backend 'html)
    (save-match-data
      (when (let ((case-fold-search t))
              (string-match
               (rx (group "<span class=\"section-number-" (+ (char digit)) "\">"
                          (+ (char digit ".")))
                   (group "</span>"))
               text))
        (replace-match "\\1.\\2"
                       t nil text)))))

(eval-after-load 'ox
  '(progn
     (add-to-list 'org-export-filter-headline-functions
                  'my-html-filter-headline-yesdot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Website Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Inspired 100% by https://ogbe.net/blog/blogging_with_org.html
;;

(defvar youtube-iframe-format
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(load "loomcom.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fixup inline images
(defun loomcom/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'loomcom/fix-inline-images)

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
                             (emacs-lisp . t)
                             (dot . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I like to navigate windows with C-<arrow-key>. These global
;; bindings do that for me.

;; 1. Make window movement fail silently when accidentally trying
;;    to navigate to where no window exists.
(defun quiet-windmove-left () (interactive) (quiet-windmove 'left))
(defun quiet-windmove-right () (interactive) (quiet-windmove 'right))
(defun quiet-windmove-up () (interactive) (quiet-windmove 'up))
(defun quiet-windmove-down () (interactive) (quiet-windmove 'down))
(defun quiet-windmove (direction)
  ;; Catch all errors and silently return nil.
  (condition-case nil
      (cond ((eq direction 'left)
             (windmove-left))
            ((eq direction 'right)
             (windmove-right))
            ((eq direction 'up)
             (windmove-up))
            ((eq direction 'down)
             (windmove-down))
            nil)
    (error nil)))

;; 2. Configure window movement keys.

;; OS X as the client
(global-set-key (read-kbd-macro "M-[ 5 D") 'quiet-windmove-left)
(global-set-key (read-kbd-macro "M-[ 5 C") 'quiet-windmove-right)
(global-set-key (read-kbd-macro "M-[ 5 A") 'quiet-windmove-up)
(global-set-key (read-kbd-macro "M-[ 5 B") 'quiet-windmove-down)
(global-set-key (read-kbd-macro "M-[ D") 'quiet-windmove-left)
(global-set-key (read-kbd-macro "M-[ C") 'quiet-windmove-right)
(global-set-key (read-kbd-macro "M-[ A") 'quiet-windmove-up)
(global-set-key (read-kbd-macro "M-[ B") 'quiet-windmove-down)

;; Linux as the client
(global-set-key (read-kbd-macro "M-[ 1 ; 5 D") 'quiet-windmove-left)
(global-set-key (read-kbd-macro "M-[ 1 ; 5 C") 'quiet-windmove-right)
(global-set-key (read-kbd-macro "M-[ 1 ; 5 A") 'quiet-windmove-up)
(global-set-key (read-kbd-macro "M-[ 1 ; 5 B") 'quiet-windmove-down)

;; Linux GTK as the client
(global-set-key (kbd "C-<left>")  'quiet-windmove-left)
(global-set-key (kbd "C-<right>") 'quiet-windmove-right)
(global-set-key (kbd "C-<up>")    'quiet-windmove-up)
(global-set-key (kbd "C-<down>")  'quiet-windmove-down)

;; Other global keys
(global-set-key "\C-xl" 'goto-line)

;; Make fonts bigger and smaller
(global-set-key (kbd "C-+")  'embiggen-default-face)
(global-set-key (kbd "C--")  'ensmallen-default-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; I publish my entire site with emacs and org-mode. org-publish is
;; horribily slow unless you disable a few modes, so I use this
;; function to accomplish things.
;;
(defun publish-loomcom ()
  "Publish my website"
  (interactive)
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
  (magit-file-mode -1)
  (global-git-gutter-mode -1)
  (org-publish-all)
  (global-git-gutter-mode +1)
  (magit-file-mode +1)
  (add-hook 'find-file-hooks 'vc-find-file-hook))

(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

;;
;; Stupid convenience functions to increase or decrease the default
;; font face height at runtime, without saving it.
;;
;; Note that C-+ and C-- do already change the face size of the
;; _current window only_, not all windows in all frames. That's
;; why I added these
;;

(defun change-face-size (dir-func delta)
  (progn
    (set-face-attribute
     'default nil :height
     (funcall dir-func (face-attribute 'default :height) delta))))

(defun embiggen-default-face ()
  (interactive)
  (change-face-size '+ 10))

(defun ensmallen-default-face ()
  (interactive)
  (change-face-size '- 10))

;;
;; Some fun functions
;;

(defun insert-clisp-project ()
  "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into
  the current buffer."
  (interactive)
  (goto-char 0)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (package (file-name-sans-extension file)))
    (insert ";;;; " file "\n\n")
    (insert "(defpackage #:" package "\n  (:use #:cl))\n\n")
    (insert "(in-package #:" package ")\n\n")))

(defun insert-html5 ()
  "Insert an HTML5 template."
  (interactive)
  (goto-char 0)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (title (file-name-sans-extension file)))
    (insert "<!doctype html>\n")
    (insert "<html lang=\"en\">\n")
    (insert "<head>\n")
    (insert "  <meta charset=\"utf-8\">\n")
    (insert "  <title>The HTML5 Herald</title>\n")
    (insert "  <meta name=\"description\" content=\"The HTML5 Herald\">\n")
    (insert "  <meta name=\"author\" content=\"SitePoint\">\n")
    (insert "  <link rel=\"stylesheet\" href=\"css/styles.css?v=1.0\">\n")
    (insert "  <!--[if lt IE 9]>\n")
    (insert "  <script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script>\n")
    (insert "  <![endif]--> \n")
    (insert "</head>\n")
    (insert "<body>\n")
    (insert "  <script src=\"js/scripts.js\"></script>\n")
    (insert "</body>\n")
    (insert "</html>\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs custom-set-variable and custom-set-faces below.
;; DO NOT HAND EDIT!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-bullets org-plus-contrib htmlize yasnippet-snippets yasnippet paredit typescript-mode git-gutter lsp-ui lsp-rust lsp-mode flycheck cargo helm haskell-mode magit treemacs graphviz-dot-mode doom-themes ledger-mode use-package)))
 '(safe-local-variable-values
   (quote
    ((eval face-remap-add-relative
           (quote org-level-3)
           (quote
            (:foreground "#B62D65" :slant italic :weight semi-bold)))
     (eval face-remap-add-relative
           (quote org-level-2)
           (quote
            (:foreground "#E27E8D" :overline t :weight semi-bold)))
     (eval face-remap-add-relative
           (quote org-level-1)
           (quote
            (:background "grey80" :foreground "grey10" :weight ultra-bold :height 1.25)))
     (eval org-content 2))))
 '(semantic-c-dependency-system-include-path
   (quote
    ("/usr/include" "/usr/include/gtk-3.0" "/usr/include/glib-2.0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

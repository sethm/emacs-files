;;; init.el --- Initialization file for Emacs
;;;
;;; Commentary:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Initialization File
;; Author: Seth Morabito <web@loomcom.com>
;; Last Updated: 30-January-2019
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minimize the UI

;;; Code:

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1))

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
(defvar desktop-dirname user-emacs-directory)
(desktop-save-mode 1)

;; Always wrap split windows
(setq truncate-partial-width-windows nil)

;; Backup and auto save. I like these to be in a unified location, not
;; scattered to the wind.

(if (not (file-exists-p "~/.emacs.d/backups/"))
    (make-directory "~/.emacs.d/backups/" t))
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
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
;; Exec Path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-exists-p "/usr/local/bin")
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))

(when (file-exists-p "/Library/TeX/texbin")
  (setq exec-path (append exec-path '("/Library/TeX/texbin")))
  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I prefer not to use custom-set-face for this, because different
;; fonts exist on different platforms. I try to apply these in order
;; of preference.

(cond ((member "Andale Mono" (font-family-list))
       (set-face-attribute 'default nil :family "Andale Mono"))
      ((member "Source Code Pro" (font-family-list))
       (set-face-attribute 'default nil :family "Source Code Pro"))
      ((member "DejaVu Sans Mono" (font-family-list))
       (set-face-attribute 'default nil :family "DejaVu Sans Mono"))
      ((member "Inconsolata" (font-family-list))
       (set-face-attribute 'default nil :family "Inconsolata"))
      (t (set-face-attribute 'default nil :family "Courier")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I load slime from Quicklisp

(when (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (defvar inferior-lisp-program "sbcl"))

;; Load some packages from local locations

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/contrib/lisp/")
(add-to-list 'load-path "~/.emacs.d/local")

;; If there is a file named 'local-init.el', load it.
(when (and (require 'mu4e nil 'noerror)
           (file-exists-p (expand-file-name "~/.emacs.d/local/local-init.el")))
  (load "local-init.el"))

;;
;; Org Mode.
;;
;; I treat org-mode specially, outside the package management
;; system, mostly for stability. I like to know exactly what
;; version I'm using at all times.
;;


(require 'org)
(require 'org-drill)
(require 'ox-rss)

(org-link-set-parameters
 "youtube"
 :follow (lambda (id)
           (browse-url
            (concat "https://www.youtube.com/embed/" id)))
 :export (lambda (path desc backend)
           (cl-case backend
             (html (format youtube-iframe-format
                           path (or desc "")))
             (latex (format "\href{%s}{%s}"
                            path (or desc "video"))))))
(setq org-pretty-entities t
      org-ellipsis "▼")

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key
             (kbd "C-c a") 'org-agenda)))

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

;; buffer-expose is not available on melpa or elpa (yet!)
(use-package buffer-expose
  :load-path "~/.emacs.d/lisp/buffer-expose"
  :config (buffer-expose-mode 1))

;; Trying out multi-term
(use-package multi-term
  :ensure t)

;; I need nice org bullets.
(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Org agenda setup varies by machine
(when (file-exists-p (expand-file-name "~/.emacs.d/local/org-agenda-setup.el"))
  (load "org-agenda-setup.el"))

;; Ledger Mode
(use-package ledger-mode
  :ensure t)

;; Themes!
(use-package cyberpunk-theme
  :ensure t)

(use-package solarized-theme
  :ensure t)

(use-package monokai-theme
  :ensure t)

(use-package monokai-alt-theme
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
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

;; web-mode
(use-package web-mode
  :ensure t
  :defer t)

;; PHP
(use-package php-mode
  :ensure t
  :defer t)

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
  (setq helm-candidate-number-limit 50
        helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t
        helm-scroll-amount 8
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  (helm-mode 1))

;; Cargo mode
(use-package cargo
  :ensure t
  :config
  (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
  (setq exec-path (append exec-path '("~/.cargo/bin"))))

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
  :hook
  (prog-mode . electric-pair-mode)
  :config
  (use-package racer
    :ensure t
    :defer t)
  (use-package flycheck
    :ensure t))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 1)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1))

(use-package lsp-mode
  :ensure t
  :config (require 'lsp-clients))

(use-package lsp-ui
  :ensure t)

(use-package toml-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp)
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

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

;; I prefer to insert periods after section numbers
;; when exporting org-mode to HTML
(defun my-html-filter-headline-yesdot (text backend info)
  "Ensure dots in headlines.
* TEXT is the text being exported.
* BACKEND is the backend (e.g. 'html).
* INFO is ignored."
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
;; Additional Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list
                            (expand-file-name "~/Projects/simh/")
                            (expand-file-name "~/Projects/simh/3B2/")))))

;; Load a theme in graphics mode.
(when (display-graphic-p)
  ;; (load-theme 'sanityinc-tomorrow-night t)
  (load-theme 'wombat t)

  )

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
  "Redisplay inline images."
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

;; `quit-windmove' exists so that I can accidentally try to move
;; somewhere that doesn't exist without Emacs freaking out.

(defun quiet-windmove-left ()
  "Navigate to the window immediately to the left the current one."
  (interactive) (quiet-windmove 'left))

(defun quiet-windmove-right ()
  "Navigate to the window immediately to the right the current one."
  (interactive) (quiet-windmove 'right))

(defun quiet-windmove-up ()
  "Navigate to the window immediately above the current one."
  (interactive) (quiet-windmove 'up))

(defun quiet-windmove-down ()
  "Navigate to the window immediately below the current one."
  (interactive) (quiet-windmove 'down))

(defun quiet-windmove (direction)
  "Catch all errors and silently return nil.
* DIRECTION is a symbol, 'left, 'right, 'up, or 'down."
  (condition-case nil
      (cond ((eq direction 'left)
             (windmove-left))
            ((eq direction 'right)
             (windmove-right))
            ((eq direction 'up)
             (windmove-up))
            ((eq direction 'down)
             (windmove-down)))
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
;; Rsync my ~/Projects/loomcom/www/ directory to my website.
;;
(defun loomcom-rsync-www ()
  "Rsync my working directory to my public web directory."
  (interactive)
  (let ((publish-dir (expand-file-name "~/Projects/loomcom/www/"))
        (remote-dir "loomcom.com:/var/www/loomcom/"))
    (when (file-exists-p publish-dir)
      (shell-command
       (format "rsync -avz --delete --delete-after %s %s" publish-dir remote-dir)))))

(defun loomcom-publish-local ()
  "Publish my website, but do not push to the server."
  (interactive)
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
  (magit-file-mode -1)
  (global-git-gutter-mode -1)
  (org-publish-all)
  (global-git-gutter-mode +1)
  (magit-file-mode +1)
  (add-hook 'find-file-hooks 'vc-find-file-hook))

;;
;; I publish my entire site with emacs and org-mode. org-publish is
;; horribily slow unless you disable a few modes, so I use this
;; function to accomplish things.
;;
(defun loomcom-publish ()
  "Publish my website."
  (interactive)
  (loomcom-publish-local)
  (loomcom-rsync-www))

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

(defun change-face-size (dir-func &optional delta)
  "Increase or decrease font size in all frames and windows.

* DIR-FUNC is a direction function (embiggen-default-face) or
  (ensmallen-default-face)
* DELTA is an amount to increase.  By default, the value is 10."
  (progn
    (set-face-attribute
     'default nil :height
     (funcall dir-func (face-attribute 'default :height) delta))))

(defun embiggen-default-face (&optional delta)
  "Increase the default font.

* DELTA is the amount (in point units) to increase the font size.
  If not specified, the dfault is 10."
  (interactive)
  (let ((incr (or delta 10)))
    (change-face-size '+ incr)))

(defun ensmallen-default-face (&optional delta)
  "Decrease the default font.

* DELTA is the amount (in point units) to decrease the font size.
  If not specified, the default is 10."
  (interactive)
  (let ((incr (or delta 10)))
    (change-face-size '- incr)))

;;
;; Some fun functions
;;

(defun insert-clisp-project ()
  "Insert a Common Lisp template into the current buffer."
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
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "d1ede12c09296a84d007ef121cd72061c2c6722fcb02cb50a77d9eae4138a3ff" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(org-bullets-bullet-list (quote ("§" "¶" "⁕" "•")))
 '(package-selected-packages
   (quote
    (multi-term spacemacs-theme color-theme-sanityinc-tomorrow monokai-alt-theme monokai-theme solarized-theme cyberpunk-theme web-mode php-mode htmlize yasnippet-snippets yasnippet paredit typescript-mode git-gutter flycheck-rust toml-mode lsp-ui lsp-mode company flycheck racer cargo helm haskell-mode magit treemacs graphviz-dot-mode doom-themes ledger-mode org-bullets use-package)))
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
     (eval org-content 2)
     (eval visual-line-mode t))))
 '(semantic-c-dependency-system-include-path
   (quote
    ("/usr/include" "/usr/include/gtk-3.0" "/usr/include/glib-2.0" "/Users/seth/Projects/simh" "/Users/seth/Projects/simh/3B2"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)

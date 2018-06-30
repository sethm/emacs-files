;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Initialization File
;; Author: Seth Morabito <web@loomcom.com>
;; Last Updated: 30-July-2018
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

;; Basic offsets
(setq-default c-basic-offset 4)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/local")
(add-to-list 'load-path "~/.emacs.d/misc")

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

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-city-lights t))

;; Treemacs
(use-package treemacs
  :ensure t)

;; magit
(use-package magit
  :ensure t)

;; Helm mode
(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50)
  (setq helm-split-window-in-side-p t)
  (setq helm-move-to-line-cycle-in-source t)
  (setq helm-ff-search-library-in-sexp t)
  (setq helm-scroll-amount 8)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-echo-input-in-header-line t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20))

;; Rust mode
(use-package rust-mode
  :ensure t
  :init
  (setq racer-cmd
        "~/.cargo/bin/racer")
  (setq racer-rust-src-path
        "~/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/"))

;; Paredit mode
(use-package paredit
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit" "Structural editing of Lisp")
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode))

;; yasnipets
(use-package yasnippet
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("~/.emacs.d/snippets"))
  (yas-global-mode t))

;; Fixup inline images
(defun loomcom/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'loomcom/fix-inline-images)

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
                             (emacs-lisp . t)
                             (dot . t)))

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))


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

;;
;; Paredit key bindings
;; --------------------
;; Paredit messes with my navigation, so I redefine several
;; paredit mode keys. Essentially, this changes C-<left>
;; and C-<right> into S-C-<left> and S-C-<right>, on multiple
;; platforms.
;;

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
  'paredit-forward-slurp-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(global-set-key (kbd "C-+")  'embiggen-default-face)
(global-set-key (kbd "C--")  'ensmallen-default-face)

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
    (insert ";;;; " file "\n")
    (insert "\n(defpackage #:" package "\n  (:use #:cl))\n\n")
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

;; Publish my org-mode 3B2 file to loomcom
(defun 3b2-publish ()
  (interactive)
  (org-html-export-as-html)
  (write-file "/seth@www.loomcom.com:/var/www/loomcom/3b2/index.html")
  (kill-buffer-and-window))


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
    (treemacs sr-speedbar sidebar-mode paredit yasnippet use-package rust-mode helm doom-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

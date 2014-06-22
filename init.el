
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc. startup options.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Configure window movement keys.
;;
;; OS X as the client
(global-set-key (read-kbd-macro "M-[ 5 D") 'quiet-windmove-left)
(global-set-key (read-kbd-macro "M-[ 5 C") 'quiet-windmove-right)
(global-set-key (read-kbd-macro "M-[ 5 A") 'quiet-windmove-up)
(global-set-key (read-kbd-macro "M-[ 5 B") 'quiet-windmove-down)
(global-set-key (read-kbd-macro "M-[ D") 'quiet-windmove-left)
(global-set-key (read-kbd-macro "M-[ C") 'quiet-windmove-right)
(global-set-key (read-kbd-macro "M-[ A") 'quiet-windmove-up)
(global-set-key (read-kbd-macro "M-[ B") 'quiet-windmove-down)
;;
;; Linux as the client
(global-set-key (read-kbd-macro "M-[ 1 ; 5 D") 'quiet-windmove-left)
(global-set-key (read-kbd-macro "M-[ 1 ; 5 C") 'quiet-windmove-right)
(global-set-key (read-kbd-macro "M-[ 1 ; 5 A") 'quiet-windmove-up)
(global-set-key (read-kbd-macro "M-[ 1 ; 5 B") 'quiet-windmove-down)
;;
;; Linux GTK as the client
(global-set-key (kbd "C-<left>")  'quiet-windmove-left)
(global-set-key (kbd "C-<right>") 'quiet-windmove-right)
(global-set-key (kbd "C-<up>")    'quiet-windmove-up)
(global-set-key (kbd "C-<down>")  'quiet-windmove-down)

;; Make emacs shut up its "No window <foo> from selected window"
;; errors when accidentally trying to move to a non-existent window.
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

;; These are required to be (interactive)
(defun quiet-windmove-left () (interactive) (quiet-windmove 'left))
(defun quiet-windmove-right () (interactive) (quiet-windmove 'right))
(defun quiet-windmove-up () (interactive) (quiet-windmove 'up))
(defun quiet-windmove-down () (interactive) (quiet-windmove 'down))

;; Show (line,column) in the modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; Turn off the annoying (to me) bell and visible bell.
(setq ring-bell-function 'ignore)

;; To just turn off visible bell but leave audible bell:
;;(setq visible-bell nil)

;; Highlight matching parens
(show-paren-mode t)

;; Transient mark mode - show hilighting when using the keyboard mark
(transient-mark-mode t)

;; Tramp wants these to be happy.  Memory is practically FREE now anyway
(setq max-lisp-eval-depth 4000)		; default is 400
(setq max-specpdl-size 5000)		; default is 1000

;; Turn off annoyances.
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(if (not (eq window-system nil))
    (tool-bar-mode -1))

;; Translates ANSI colors in shell.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq-default java-indent 4)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (local-set-key  (kbd "C-c o") 'ff-find-other-file)
             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil)))

(add-hook 'sh-set-shell-hook
          '(lambda ()
             (setq sh-basic-offset 4)))

(add-hook 'python-mode-hook
	  '(lambda ()
             (setq python-indent 4)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq ruby-indent 2)
            (setq c-basic-offset 2)))

;; The Go style guide says tabs, so tabs it is.
(add-hook 'go-mode-hook
          '(lambda ()
             (setq indent-tabs-mode t)
             (setq tab-width 8)))

;; Tell dired to hide dot files and emacs backup files.
(add-hook 'dired-load-hook
          '(lambda ()
             (load "dired-x")))

(add-hook 'dired-mode-hook
          '(lambda ()
             (setq dired-omit-files "^\\.[a-z|A-Z]+\\|^\\.?#\\|^\\.$")
             (dired-omit-mode 1)))

;; Enable upcase-region function (why is this disabled by default??)
(put 'upcase-region 'disabled nil)

;; Font Lock
(global-font-lock-mode t)
(display-time-mode t)

;; Enable visual feedback on selections
(setq transient-mark-mode t)

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

;; Tell emacs where to find custom themes.
(setq custom-theme-directory "~/.emacs.d/themes/")

;; Always auto-revert buffers if they are un-edited, and the file changes
;; on disk.
(global-auto-revert-mode t)

;; NB: This is NOT SAFE IN GENERAL, custom themes can run arbitrary
;; code. But since I control my own 'themes' directory, I'm going to
;; run with scissors and make all custom themes safe by default.
(setq custom-safe-themes t)

;; Delete trailing whitespace on saves
; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save desktop state.
(setq desktop-dirname             "~/.emacs.d/local/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)


;; I'm kind of a dummy, and I need this :B
(setq confirm-kill-emacs 'yes-or-no-p)

(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/local")
(add-to-list 'load-path "~/.emacs.d/misc")

;;
;; Emacs built-in package management and the Marmalade repo.
;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages '(ac-nrepl
                      auto-complete
                      cider
                      clojure-cheatsheet
                      clojure-mode
                      clojure-snippets
                      clojure-test-mode
                      coffee-mode
                      discover
                      dsvn
                      elnode
                      git-commit
                      go-mode
                      graphviz-dot-mode
                      haml-mode
                      json
                      magit
                      markdown-mode
                      multi-term
                      pg
                      rainbow-delimiters
                      request
                      rinari
                      ruby-mode
                      rvm
                      scss-mode
                      starter-kit
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-js
                      starter-kit-lisp
                      starter-kit-ruby
                      textmate
                      twittering-mode
                      verilog-mode
                      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; This is a super annoying feature, sometimes. Turn it off
(setq ido-use-filename-at-point nil)

(global-ede-mode 1)
(semantic-mode 1)
(global-semantic-decoration-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-idle-summary-mode 1)

;; paredit-mode tries to snag C-<left> and C-<right> when editing
;; LISP. It maps the same to C-S-<left> and C-S-<right>, so just keep
;; those but kill off the ones I like to use.

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-<left>") nil)
     (define-key paredit-mode-map (kbd "C-<right>") nil)
     (define-key paredit-mode-map (kbd "C-S-<left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-S-<right>") 'paredit-forward-slurp-sexp)

     ;; But then we have the same problem as above. When running in a
     ;; terminal, we need to capture weird escape sequences for C-S-<left>
     ;; and C-S-<right>. Oh bother.

     ;; OS X as the client
     (define-key paredit-mode-map (read-kbd-macro "S-M-[ 5 D") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (read-kbd-macro "S-M-[ 5 C") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (read-kbd-macro "M-[ 1 ; 6 d") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (read-kbd-macro "M-[ 1 ; 6 c") 'paredit-forward-slurp-sexp)

     ;; Linux as the client
     (define-key paredit-mode-map (read-kbd-macro "S-M-[ 1 ; 5 D") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (read-kbd-macro "S-M-[ 1 ; 5 C") 'paredit-forward-slurp-sexp)))

;; Multi-Term mode

(setq multi-term-program
      (cond ((file-exists-p "/bin/bash") "/bin/bash")
            ((file-exists-p "/usr/local/bin/bash") "/usr/local/bin/bash")))

(setq explicit-shell-file-name
      (cond ((file-exists-p "/bin/bash") "/bin/bash")
            ((file-exists-p "/usr/local/bin/bash") "/usr/local/bin/bash")))

(if explicit-shell-file-name
    (progn
      (setq explicit-bash-args '("--noediting" "--login" "-i"))
      (setenv "SHELL" shell-file-name)
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

;; I can't believe semantic-ia-fast-jump doesn't have a default key
;; binding. It's the single most useful part of semantic-mode!
(define-key semantic-mode-map (kbd "C-c , >") 'semantic-ia-fast-jump)

(require 'mud)

;; Apple LLDB-aware Grand Unified Debugger

(require 'gud)

(require 'nasm-mode)
(add-to-list 'auto-mode-alist '("\\.S\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.a65\\'" . nasm-mode))

;; Verilog mode
(setq verilog-auto-lineup nil
      verilog-auto-newline nil)

;; SCons
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))


;; twittering-mode
(setq twittering-use-master-password t)

;; mu4e (Don't ask. Machine differences.)
(setq mu4e-load-path nil)

(cond ((file-exists-p "/opt/share/emacs/site-lisp/mu4e")
       (setq mu4e-load-path "/opt/share/emacs/site-lisp/mu4e"))
      ((file-exists-p "/usr/local/share/emacs/site-lisp/mu4e")
       (setq mu4e-load-path "/usr/local/share/emacs/site-lisp/mu4e")))

(if mu4e-load-path
    (progn
      (add-to-list 'load-path mu4e-load-path)
      (require 'mu4e)))

;; Gnus and Mail are in a local directory, not checked in.
(if (file-exists-p (expand-file-name "~/.emacs.d/local/mail-and-news.el"))
    (load "mail-and-news"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map "\C-xj" 'java-mode)
(global-set-key "\C-xl" 'goto-line)
(global-set-key "\C-cl" 'linum-mode)

;; Always enable 'discover' mode.
(discover-mode)

;; Auto-complete goodness
(global-auto-complete-mode)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-mode-alist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append
       '(("\\.text$" . text-mode)
         ("\\.txt$" . text-mode)
         ("\\.tl$" . ruby-mode)
         ("\\.py$" . python-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.el$" . emacs-lisp-mode)
         ("\\.pl$" . perl-mode)
         ("\\.pm$" . perl-mode)
         ("\\.c$" . c-mode)
         ("\\.h$" . c-mode)
         ("\\.outline$" . outline-mode)
         ("\\.lisp$" . lisp-mode)
         ("\\.java$" . java-mode)) auto-mode-alist))

;; Only set mode-line face if running in a terminal
(if (eq window-system nil)
    (progn
      (custom-set-faces
       '(mode-line ((t (:foreground "cyan" :inverse-video t))))
       '(mode-line-inactive ((default (:inherit mode-line)) (nil (:foreground "white"))))
       ))

  ;; Otherwise, we're running in a windowed environment
  (progn
    ;; Window system is Mac OS X ("Emacs for OS X"), use Menlo
    (if (string= window-system "ns")
        (add-to-list 'default-frame-alist '(font . "Menlo-14"))
      ;; Otherwise, use Inconsolata
      (set-frame-font "Inconsolata-12"))

    (load-theme 'loomcom)
    (normal-erase-is-backspace-mode 1)))

;; Load C includes (defined on a per-environment basis, in my "local"
;; subdirectory)

(if (file-exists-p (expand-file-name "~/.emacs.d/local/c-includes.el"))
    (load "c-includes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Stupid convenience functions to increase or decrease the default
;; font face height at runtime, without saving it.
;;
;; Note that C-+ and C-- do already change the face size of the
;; _current window only_, not all windows in all frames. That's
;; why I added these
;;

(defun change-default-face-size (dir-func delta)
  (set-face-attribute 'default nil :height
                      (funcall dir-func (face-attribute 'default :height) delta)))

(defun embiggen-default-face ()
  (interactive)
  (change-default-face-size '+ 10))

(defun ensmallen-default-face ()
  (interactive)
  (change-default-face-size '- 10))

(global-set-key (kbd "C-+")  'embiggen-default-face)
(global-set-key (kbd "C--")  'ensmallen-default-face)

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

(defun insert-xhtml-1 ()
  "Insert a template XHTML 1.0 transitional snippet into
  the current buffer"
  (interactive)
  (goto-char 0)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (title (file-name-sans-extension file)))
    (insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n")
    (insert "      \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")
    (insert "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n")
    (insert "  <head>\n")
    (insert "    <title>" title "</title>\n")
    (insert "  </head>\n")
    (insert "  <body>\n")
    (insert "  </body>\n")
    (insert "</html>\n")))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-offsets-alist (quote ((innamespace . [0]))))
 '(find-grep-options "-q -I")
 '(menu-bar-mode t)
 '(org-fontify-whole-heading-line t)
 '(org-hide-leading-stars nil)
 '(org-src-fontify-natively t)
 '(org-startup-folded nil)
 '(require-final-newline nil))

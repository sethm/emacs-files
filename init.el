
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

;; A hack. My beloved Kinesis Advantage keyboard at work has started
;; freaking out and sending the wrong keys for F3 and F4. I need
;; these. I can't live without kmacro-start-macro-or-insert-counter
;; and kmacro-end-or-call-macro. So I have no choice but to provide
;; alternate mappings until I get my keyboard fixed.

(global-set-key (kbd "C-c (") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-c )") 'kmacro-end-or-call-macro)

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

(add-hook 'sh-set-shell-hook
          (lambda ()
            (setq tab-width 4)
            (setq c-basic-offset 4)
            (setq sh-basic-offset 4)))

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq tab-width 4)
            (setq python-indent 4)
            (setq c-basic-offset 4)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq ruby-indent 2)
            (setq c-basic-offset 2)))

;; The Go style guide says tabs, so tabs it is.
(add-hook 'go-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)))

(add-hook 'java-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq java-indent 4)
            (setq c-basic-offset 4)))


;; Fix tabs
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; Tell dired to hide dot files and emacs backup files.
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))
(add-hook 'dired-mode-hook
          (lambda ()
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

;; NB: This is NOT SAFE IN GENERAL, custom themes can run arbitrary
;; code. But since I control my own 'themes' directory, I'm going to
;; run with scissors and make all custom themes safe by default.
(setq custom-safe-themes t)

;; GAS mode settings
(add-hook 'gas-mode-hook '(lambda ()
			    (setq tab-width 8)
			    (setq gas-opcode-column 16)
			    (setq gas-argument-column 24)
			    (setq gas-comment-column 40)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq path "/usr/local/bin:/bin:/sbin:/usr/bin:/usr/sbin")
(setenv "PATH" path)

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/share/npm/bin/")
(add-to-list 'exec-path "/usr/local/bin")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-ede-mode 1)
(semantic-mode 1)
(global-semantic-decoration-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-idle-summary-mode 1)

;; If I'm on my work machine, set up my additional includes.

(let ((work-root (expand-file-name "~/Work")))
  (if (file-exists-p work-root)
      (progn
        (semantic-add-system-include (format "%s/%s" work-root "common/head/lib/nom/include/") 'c++-mode)
        (semantic-add-system-include (format "%s/%s" work-root "libnomxx/head/") 'c++-mode)
        (semantic-add-system-include (format "%s/%s" work-root "boost/1.51.0.0.4/boost_1_51_0") 'c++-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; el-get
;;
;; Any machine I copy my emacs config to will bootsrap its own el-get
;; packages when needed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(setq el-get-user-package-directory "~/.emacs.d/el-get-package-init/")

;; If el-get is not installed, pull it down, install it, and then synchronize
;; all configured packages from el-get-sources

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; Request a specific source for yasnippet
(setq el-get-sources
      '((:name yasnippet
	       :website "https://github.com/capitaomorte/yasnippet.git"
	       :description "YASnippet is a template system for Emacs."
	       :type github
	       :pkgname "capitaomorte/yasnippet"
	       :features "yasnippet"
	       :compile "yasnippet.el")))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(el-get 'sync)

;; List of packages to auto-install

(setq my-packages
      '(el-get
	yasnippet
        ruby-compilation
        ruby-mode
        css-mode
        haml-mode
        coffee-mode
        inf-ruby
        rhtml-mode
        rvm
        textmate
        yaml-mode
        dsvn
        scss-mode
        git-commit-mode
        magit
        magithub
        go-mode
	twittering-mode))

(el-get 'sync my-packages)

;;
;; gas-mode is not supported by el-get, so it must be loaded manually.
;; 
(require 'gas-mode)
(add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))
(add-to-list 'auto-mode-alist '("\\.asm\\'" . gas-mode))
(add-to-list 'auto-mode-alist '("\\.a65\\'" . gas-mode))

;;
;; EasyPG
;;
(require 'epa-file)
(epa-file-enable)

;;
;; twittering-mode
;;
(setq twittering-use-master-password t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Slime
;;
;; Slime is an exception to el-get, because I use 'quicklisp' to
;; manage my slime dependencies
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (and (file-exists-p "~/quicklisp/slime-helper.el")
	 (file-exists-p "/usr/local/bin/sbcl"))
    (progn
      (load (expand-file-name "~/quicklisp/slime-helper.el"))
      (setq inferior-lisp-program "/usr/local/bin/sbcl")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map "\C-xj" 'java-mode)
(global-set-key "\C-xl" 'goto-line)
(global-set-key "\C-cl" 'linum-mode)

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
	(set-frame-font "Menlo-14")
      ;; Otherwise, use Inconsolata
      (set-frame-font "Inconsolata-14"))

    (load-theme 'loomcom)
    (normal-erase-is-backspace-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun catalog ()
  (interactive)
  (find-file "/seth@dev-sb01.glyde.com:~/Work/catalog/"))

(defun trunk ()
  (interactive)
  (find-file "/seth@dev-sb01.glyde.com:~/Work/trunk/"))

(defun branch ()
  (interactive)
  (find-file "/seth@dev-sb01.glyde.com:~/Work/branch/"))

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

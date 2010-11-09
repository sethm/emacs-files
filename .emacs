;; .emacs


;; defaults write org.gnu.Emacs Emacs.font "-*-bitstream vera sans mono-medium-r-normal--14-*-*-*-*-*-mac-roman"
;; defaults write org.gnu.Emacs Emacs.font "-apple-monaco-medium-r-normal--14-*-*-*-*-*-mac-roman"

;; (setq mac-allow-anti-aliasing nil)  ;; turn off anti-aliasing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc. startup options.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;;;; Configure window movement keys.
;;
;; OS X as the client
(global-set-key (read-kbd-macro "M-[ 5 D") 'windmove-left)
(global-set-key (read-kbd-macro "M-[ 5 C") 'windmove-right)
(global-set-key (read-kbd-macro "M-[ 5 A") 'windmove-up)
(global-set-key (read-kbd-macro "M-[ 5 B") 'windmove-down)
;;
;; Linux as the client
(global-set-key (read-kbd-macro "M-[ 1 ; 5 D") 'windmove-left)
(global-set-key (read-kbd-macro "M-[ 1 ; 5 C") 'windmove-right)
(global-set-key (read-kbd-macro "M-[ 1 ; 5 A") 'windmove-up)
(global-set-key (read-kbd-macro "M-[ 1 ; 5 B") 'windmove-down)
;;
;; Linux GTK as the client
(global-set-key (kbd "C-<left>")  'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<up>")    'windmove-up)
(global-set-key (kbd "C-<down>")  'windmove-down)

;; Show (line,column) in the modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; Transient mark mode - show hilighting when using the keyboard mark
(transient-mark-mode t)

;; Tramp wants these to be happy.  Memory is practically FREE now anyway
(setq max-lisp-eval-depth 4000)		; default is 400
(setq max-specpdl-size 5000)		; default is 1000

;; Turn off annoyances.
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Translates ANSI colors in shell.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Fix tabs
(setq indent-tabs-mode nil)
(setq ruby-indent-tabs-mode nil)
(setq inhibit-default-init t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up load paths.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime/contrib/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/cedet/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/ecb/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/rinari/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/yasnippet/"))

;; Outline mode
(add-to-list 'auto-mode-alist '("\\.outline$" . outline-mode))

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")

;; IDO (Interactively DO mode)
(require 'ido)
(ido-mode t)

;; Rinari
(require 'rinari)
(setq rinari-tags-file-name "TAGS")

;;
;; Ruby Mode.
;;
(autoload 'ruby-mode "~/.emacs.d/ruby-mode"
  "Mode for editing ruby source files" t)

(add-hook 'ruby-mode-hook
	  '(lambda ()
             (setq c-basic-offset 2)
             (setq tab-width 2)
             (setq indent-tabs-mode nil)))

;;
;; Java mode
;;
(add-hook 'java-mode-hook
          '(lambda ()
             (setq c-basic-offset 2)
             (setq tab-width 2)
             (setq indent-tabs-mode nil)))

;; handle @annotations in Java mode.
(add-hook 'java-mode-hook
          '(lambda () "Treat Java 1.5 @-style annotations as comments."
             (setq c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
             (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;;
;; HAML Mode.
;;
(autoload 'haml-mode "~/.emacs.d/haml-mode"
  "Mode for editing haml templates" t)

;; CEDET
(load-file "~/.emacs.d/cedet/common/cedet.el")
(require 'cedet)

;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; Select one of the following:

;; * This enables the database and idle reparse engines
;;(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;; (semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as the nascent intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-guady-code-helpers)

;; * This turns on which-func support (Plus all other code helpers)
(semantic-load-enable-excessive-code-helpers)
(setq semanticdb-default-save-directory "~/.semantic.cache")

;; This turns on modes that aid in grammar writing and semantic tool
;; development.  It does not enable any other features such as code
;; helpers above.
;; (semantic-load-enable-semantic-debugging-helpers)

;; Emacs Code Browser
(require 'ecb-autoloads)

;; CSS mode
(add-hook 'css-mode-hook
          '(lambda ()
             (turn-on-lazy-lock)
             (setq c-basic-offset 4 tab-width 4)
             (setq indent-tabs-mode t)))

;; SLIME
(setq inferior-lisp-program "/usr/bin/env sbcl")
(require 'slime)
(slime-setup '(slime-fancy slime-banner slime-repl))

;; YAML
(require 'yaml-mode)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-set-variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cssm-indent-level 4)
 '(default-frame-alist (quote ((menu-bar-lines . 1) (background-mode . light))))
 '(ecb-auto-activate t)
 '(ecb-layout-name "left13")
 '(ecb-options-version "2.32")
 '(ecb-source-path (quote ("/seth@dev.glyde.com:/Users/seth/Work/trunk")))
 '(ecb-tip-of-the-day nil)
 '(ecb-vc-enable-support t)
 '(explicit-bash-args (quote ("--noediting" "-i" "-l")))
 '(graphviz-dot-indent-width 2)
 '(javascript-indent-level 2)
 '(menu-bar-mode t)
 '(shell-completion-execonly t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)))
 '(version-control t))


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
	 ("\\.rb$" . ruby-mode)
	 ("\\.tl$" . ruby-mode)
	 ("Rakefile$" . ruby-mode)
	 ("\\.rake$" . ruby-mode)
	 ("\\.py$" . python-mode)
	 ("\\.css$" . java-mode)
	 ("\\.haml$" . haml-mode)
	 ("\\.emacs$" . emacs-lisp-mode)
	 ("\\.el$" . emacs-lisp-mode)
	 ("\\.pl$" . perl-mode)
	 ("\\.pm$" . perl-mode)
	 ("\\.c$" . c-mode)
	 ("\\.h$" . c-mode)
	 ("\\.yml$" . yaml-mode)
	 ("\\.lisp$" . lisp-mode)
	 ("\\.ru$" . ruby-mode)
	 ("\\.java$" . java-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-set-faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 8)) nil))))

;; Only set mode-line face if running in a terminal
(if (eq window-system nil)
    (progn
      (custom-set-faces
       '(mode-line ((t (:foreground "cyan" :inverse-video t))))
       '(mode-line-inactive ((default (:inherit mode-line)) (nil (:foreground "white"))))
       )))

;; Window system is Mac OS X ("Emacs for OS X")
(if (string= window-system "ns")
  (progn
    (normal-erase-is-backspace-mode 1)
    (set-default-font "-apple-inconsolata-medium-r-normal--17-170-72-72-m-170-utf-8")
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun catalog ()
  (interactive)
  (find-file "/seth@dev-sb01.glyde.com:~/Work/catalog/")
  )

(defun trunk ()
  (interactive)
  (find-file "/seth@dev-sb01.glyde.com:~/Work/trunk/")
  )

(defun branch ()
  (interactive)
  (find-file "/seth@dev-sb01.glyde.com:~/Work/branch/")
  )

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


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

;; Transient mark mode - show hilighting when using the keyboard mark
(transient-mark-mode t)

;; Tramp wants these to be happy.  Memory is practically FREE now anyway
(setq max-lisp-eval-depth 4000)		; default is 400
(setq max-specpdl-size 5000)		; default is 1000

;; Turn off annoyances.
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(tool-bar-mode -1)

;; Mumamo makes emacs 23.3 and higher complain about obsolete
;; variables.  This is a sneaky workaround.
(when (and (equal emacs-major-version 23)
           (> emacs-minor-version 2))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function)))  

;; Translates ANSI colors in shell.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'sh-set-shell-hook
          (lambda ()
            (setq tab-width 4)
            (setq c-basic-offset 4)
            (setq sh-basic-offset 4)))

;; Fix tabs
(setq indent-tabs-mode nil)
(setq c-basic-offset 2)
(setq tab-width 2)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/haml-mode/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/rhtml/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/rvm.el/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/textmate.el/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/yaml-mode/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/yasnippet/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/inf-ruby/"))

;; Ruby Mode
(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-hook 'ruby-mode-hook '(lambda ()
                             (setq ruby-deep-arglist t)
                             (setq ruby-deep-indent-paren nil)
                             (setq c-tab-always-indent nil)
                             (require 'inf-ruby)
                             (require 'ruby-compilation)))

;; rhtml Mode
(autoload 'rhtml-mode "rhtml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
(add-hook 'rhtml-mode '(lambda ()
                         (define-key rhtml-mode-map (kbd "M-s") 'save-buffer)))

;; yaml mode
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; css mode
(autoload 'css-mode "css-mode" nil t)
(add-hook 'css-mode-hook '(lambda ()
                            (setq css-indent-level 2)
                            (setq css-indent-offset 2)))

;; haml mode
(autoload 'haml-mode "haml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;; textmate
(autoload 'textmate-mode "textmate" nil t)
(textmate-mode)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

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
         ("\\.lisp$" . lisp-mode)
         ("\\.java$" . java-mode)) auto-mode-alist))

;; Only set mode-line face if running in a terminal
(if (eq window-system nil)
    (progn
      (custom-set-faces
       '(mode-line ((t (:foreground "cyan" :inverse-video t))))
       '(mode-line-inactive ((default (:inherit mode-line)) (nil (:foreground "white"))))
       )))

;; Window system is Mac OS X ("Emacs for OS X"),
(if (string= window-system "ns")
    (progn
      (set-frame-font "Menlo-14")
      (load-theme 'tango)
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


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

(add-hook 'ca65-mode-hook
          '(lambda ()
             (setq tab-width 8)
             (setq indent-tabs-mode t)))

(add-hook 'asm-mode-hook
          '(lambda ()
             (setq tab-width 8)
             (setq indent-tabs-mode t)))

(add-hook 'sh-set-shell-hook
          '(lambda ()
             (setq sh-basic-offset 4)))

(add-hook 'python-mode-hook
          '(lambda ()
             (setq python-indent 4)))

(add-hook 'ruby-mode-hook
          '(lambda ()
            (setq tab-width 2)
            (setq ruby-indent 2)
            (setq c-basic-offset 2)))

(add-hook 'js-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq js-indent-level 2)
             (setq c-basic-offset 2)))

(add-hook 'js2-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq js2-basic-offset 2)
             (setq c-basic-offset 2)
             (disable-paredit-mode)))

;; The Go style guide says tabs, so tabs it is.
(add-hook 'go-mode-hook
          '(lambda ()
             (setq indent-tabs-mode t)
             (setq tab-width 4)))

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

;; ;; Save desktop state.
;; (setq desktop-dirname             "~/.emacs.d/local/"
;;       desktop-base-file-name      "emacs.desktop"
;;       desktop-base-lock-name      "lock"
;;       desktop-path                (list desktop-dirname)
;;       desktop-save                t
;;       desktop-files-not-to-save   "^$" ;reload tramp paths
;;       desktop-load-locked-desktop nil)
;;
;; (desktop-save-mode 1)

;; I'm kind of a dummy, and I need this :B
(setq confirm-kill-emacs 'yes-or-no-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/local")
(add-to-list 'load-path "~/.emacs.d/misc")

;;
;; Emacs built-in package management and the Marmalade repo.
;;

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(defvar my-packages '(ac-nrepl
                      auto-complete
                      better-defaults
                      cargo
                      cider
                      coffee-mode
                      company
                      company-racer
                      discover
                      dsvn
                      elnode
                      exec-path-from-shell
                      find-file-in-project
                      fic-mode
                      fill-column-indicator
                      git-commit
                      git-timemachine
                      git-gutter
                      go-mode
                      graphviz-dot-mode
                      groovy-mode
                      haml-mode
                      haskell-mode
                      idle-highlight-mode
                      ido-ubiquitous
                      magit
                      markdown-mode
                      multi-term
                      multiple-cursors
                      org-bullets
                      paredit
                      pg
                      rainbow-delimiters
                      racer
                      request
                      rust-mode
                      rvm
                      scss-mode
                      scpaste
                      smex
                      toml-mode
                      textmate
                      toml-mode
                      typescript-mode
                      verilog-mode
                      vue-mode
                      web-mode
                      xml-rpc
                      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Org-jira mode
(if (file-exists-p (expand-file-name "~/.emacs.d/local/jira.el"))
    (load "jira"))

;; This is a super annoying feature, sometimes. Turn it off
(setq ido-use-filename-at-point nil)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(global-ede-mode 1)
(semantic-mode 1)
(global-semantic-decoration-mode 1)
(global-semantic-stickyfunc-mode 1)
(global-semantic-idle-summary-mode 1)

(global-git-gutter-mode +1)

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

;; Make sure PATH environment variable works
(exec-path-from-shell-initialize)

;; yasnipets
(add-to-list 'auto-mode-alist '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)

;; Org mode should have nice bullets.
(add-hook 'org-mode-hook 'org-bullets-mode)

;; Rust-mode
(add-hook 'rust-mode-hook 'electric-pair-mode)

(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/rust/src")

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;; YOW! A can of ASPARAGUS, 73 pigeons, some LIVE ammo, and a FROZEN
;; DAQUIRI!!
(require 'yow)
(setq yow-file "~/.emacs.d/misc/yow.txt.gz")

(require 'fireplace)

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

;; Web mode
(setq web-mode-markup-indent-offset 2)

;; fill-column-mode
(setq fci-rule-column 80)

;; Multiple-Cursors mode
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Slime! I let Quicklisp handle slime for me.

(if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
    (progn
      (load (expand-file-name "~/quicklisp/slime-helper.el"))
      ;; Replace "sbcl" with the path to your implementation
      (setq inferior-lisp-program "sbcl")
      (require 'slime)
      (slime-setup '(slime-fancy slime-tramp slime-asdf))
      (slime-require :swank-listener-hooks)))

;; Haskell mode hook
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(require 'ca65-mode)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . ca65-mode))
(add-to-list 'auto-mode-alist '("\\.a65\\'" . ca65-mode))

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
         ("\\.py$" . python-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.el$" . emacs-lisp-mode)
         ("\\.pl$" . perl-mode)
         ("\\.pm$" . perl-mode)
         ("\\.c$" . c-mode)
         ("\\.h$" . c-mode)
         ("\\.outline$" . outline-mode)
         ("\\.lisp$" . lisp-mode)
         ("\\.js$" . js2-mode)
         ("\\.tsx?$" . typescript-mode)
         ("\\.cs$" . csharp-mode)
         ("\\.vue$" . vue-mode)
         ("\\.java$" . java-mode)) auto-mode-alist))

;; Load C includes (defined on a per-environment basis, in my "local"
;; subdirectory)

(if (file-exists-p (expand-file-name "~/.emacs.d/local/c-includes.el"))
    (load "c-includes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (set-face-attribute 'default nil :height
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

(load-theme 'loomcom t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook (quote (delete-trailing-whitespace)))
 '(c-offsets-alist (quote ((innamespace . +))))
 '(find-grep-options "-q -I")
 '(ido-use-virtual-buffers t)
 '(menu-bar-mode t)
 '(org-fontify-whole-heading-line t)
 '(org-hide-leading-stars nil)
 '(org-pretty-entities t)
 '(org-src-fontify-natively t)
 '(org-startup-folded nil)
 '(org-startup-indented nil)
 '(package-selected-packages
   (quote
    (vue-mode fill-column-indicator fic-mode racer company-racer company yasnippet yaml-mode web-mode twittering-mode toml-mode textmate scss-mode rvm request rainbow-delimiters quack pg org-bullets multiple-cursors multi-term markdown-mode js2-mode haskell-mode haml-mode groovy-mode graphviz-dot-mode go-mode git-timemachine git-gutter geiser exec-path-from-shell elnode dsvn discover csharp-mode coffee-mode cargo ac-nrepl)))
 '(require-final-newline nil)
 '(sml/theme (quote automatic))
 '(vc-git-diff-switches t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "dark blue" :foreground "white" :box nil))))
 '(org-block-background ((t (:background "grey20"))))
 '(org-block-begin-line ((t (:inverse-video t :height 0.6))) t)
 '(org-block-end-line ((t (:inverse-video t :height 0.6))) t))

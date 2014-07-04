;;
;; Generic asm-mode, modified slightly for the CC65 toolchain.
;;

(defgroup asm6502 nil
  "Mode for editing 6502 assembler code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom asm6502-comment-char ?\;
  "The comment-start character assumed by Asm6502 mode."
  :type 'character
  :group 'asm)

(defvar asm6502-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    st)
  "Syntax table used while in Asm mode.")

(defvar asm6502-mode-abbrev-table nil
  "Abbrev table used while in Asm6502 mode.")
(define-abbrev-table 'asm6502-mode-abbrev-table ())

(defvar asm6502-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Note that the comment character isn't set up until asm6502-mode is called.
    (define-key map ":"		'asm6502-colon)
    (define-key map "\C-c;"	'comment-region)
    (define-key map "\C-j"	'newline-and-indent)
    (define-key map "\C-m"	'newline-and-indent)
    (define-key map [menu-bar asm6502-mode] (cons "Asm" (make-sparse-keymap)))
    (define-key map [menu-bar asm6502-mode comment-region]
      '(menu-item "Comment Region" comment-region
		  :help "Comment or uncomment each line in the region"))
    (define-key map [menu-bar asm6502-mode newline-and-indent]
      '(menu-item "Insert Newline and Indent" newline-and-indent
		  :help "Insert a newline, then indent according to major mode"))
    (define-key map [menu-bar asm6502-mode asm6502-colon]
      '(menu-item "Insert Colon" asm6502-colon
		  :help "Insert a colon; if it follows a label, delete the label's indentation"))
    map)
  "Keymap for Asm mode.")

(defconst asm6502-font-lock-keywords
  (append
   '(("^\\(\\(\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
      (1 font-lock-function-name-face) (3 font-lock-keyword-face nil t))
     ("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
      (1 font-lock-function-name-face) (3 font-lock-keyword-face nil t))
     ("^\\(\\@\\(\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
      (1 font-lock-function-name-face) (3 font-lock-keyword-face nil t))
     ;; label started from ".".
     ("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>:"
      1 font-lock-function-name-face)
     ;; label started from "@".
     ("^\\(\\@\\(\\sw\\|\\s_\\)+\\)\\>:"
      1 font-lock-function-name-face)
     ("^\\((\\sw+)\\)?\\s +\\(\\(\\.?\\sw\\|\\s_\\)+\\(\\.\\sw+\\)*\\)"
      2 font-lock-keyword-face)
     ;; directive started from ".".
     ("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>[^:]?"
      1 font-lock-keyword-face)
     ;; %register
     ("%\\sw+" . font-lock-variable-name-face))
   cpp-font-lock-keywords)
  "Additional expressions to highlight in 6502 Assembler mode.")

;;;###autoload
(define-derived-mode asm6502-mode prog-mode "6502"
  "Major mode for editing 6502 assembler code.
Features a private abbrev table and the following bindings:

\\[asm6502-colon]\toutdent a preceding label, tab to next tab stop.
\\[tab-to-tab-stop]\ttab to next tab stop.
\\[asm6502-newline]\tnewline, then tab to next tab stop.
\\[asm6502-comment]\tsmart placement of assembler comments.

The character used for making comments is set by the variable
`asm6502-comment-char' (which defaults to `?\\;').

Alternatively, you may set this variable in `asm6502-mode-set-comment-hook',
which is called near the beginning of mode initialization.

Turning on Asm mode runs the hook `asm6502-mode-hook' at the end of initialization.

Special commands:
\\{asm6502-mode-map}"
  (setq local-abbrev-table asm6502-mode-abbrev-table)
  (set (make-local-variable 'font-lock-defaults) '(asm6502-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'asm6502-indent-line)
  ;; Stay closer to the old TAB behavior (was tab-to-tab-stop).
  (set (make-local-variable 'tab-always-indent) nil)

  (run-hooks 'asm6502-mode-set-comment-hook)
  ;; Make our own local child of asm6502-mode-map
  ;; so we can define our own comment character.
  (use-local-map (nconc (make-sparse-keymap) asm6502-mode-map))
  (local-set-key (vector asm6502-comment-char) 'asm6502-comment)
  (set-syntax-table (make-syntax-table asm6502-mode-syntax-table))
  (modify-syntax-entry	asm6502-comment-char "< b")

  (set (make-local-variable 'comment-start) (string asm6502-comment-char))
  (set (make-local-variable 'comment-add) 1)
  (set (make-local-variable 'comment-start-skip)
       "\\(?:\\s<+\\|/[/*]+\\)[ \t]*")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(\\s>\\|\\*+/\\)")
  (set (make-local-variable 'comment-end) "")
  (setq fill-prefix "\t"))

(defun asm6502-indent-line ()
  "Auto-indent the current line."
  (interactive)
  (let* ((savep (point))
	 (indent (condition-case nil
		     (save-excursion
		       (forward-line 0)
		       (skip-chars-forward " \t")
		       (if (>= (point) savep) (setq savep nil))
		       (max (asm6502-calculate-indentation) 0))
		   (error 0))))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun asm6502-calculate-indentation ()
  (or
   ;; Flush labels to the left margin.
   (and (looking-at "\\(\\sw\\|\\s_\\)+:") 0)
   ;; Labels starting with @
   (and (looking-at "\\@\\(\\sw\\|\\s_\\)+:") 0)
   ;; Labels starting with .
   (and (looking-at "\\.\\(\\sw\\|\\s_\\)+:") 0)
   ;; Same thing for `;;;' comments.
   (and (looking-at "\\s<\\s<\\s<") 0)
   ;; Simple `;' comments go to the comment-column.
   (and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
   ;; The rest goes at the first tab stop.
   (or (indent-next-tab-stop 0))))

(defun asm6502-colon ()
  "Insert a colon; if it follows a label, delete the label's indentation."
  (interactive)
  (let ((labelp nil))
    (save-excursion
      (skip-syntax-backward "w_.")
      (skip-syntax-backward " ")
      (if (setq labelp (bolp)) (delete-horizontal-space)))
    (call-interactively 'self-insert-command)
    (when labelp
      (delete-horizontal-space)
      (tab-to-tab-stop))))

;; Obsolete since Emacs-22.1.
(defalias 'asm6502-newline 'newline-and-indent)

(defun asm6502-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

   1 -- comment to the right of the code (at the comment-column)
   2 -- comment on its own line, indented like code
   3 -- comment on its own line, beginning at the left-most column.

Suggested usage:  while writing your code, trigger asm6502-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (comment-normalize-vars)
  (let (comempty comment)
    (save-excursion
      (beginning-of-line)
      (with-no-warnings
	(setq comment (comment-search-forward (line-end-position) t)))
      (setq comempty (looking-at "[ \t]*$")))

  (cond

   ;; Blank line?  Then start comment at code indent level.
   ;; Just like `comment-dwim'.  -stef
   ((save-excursion (beginning-of-line) (looking-at "^[ \t]*$"))
    (indent-according-to-mode)
    (insert asm6502-comment-char asm6502-comment-char ?\ ))

   ;; Nonblank line w/o comment => start a comment at comment-column.
   ;; Also: point before the comment => jump inside.
   ((or (null comment) (< (point) comment))
    (indent-for-comment))

   ;; Flush-left or non-empty comment present => just insert character.
   ((or (not comempty) (save-excursion (goto-char comment) (bolp)))
    (insert asm6502-comment-char))

   ;; Empty code-level comment => upgrade to next comment level.
   ((save-excursion (goto-char comment) (skip-chars-backward " \t") (bolp))
    (goto-char comment)
    (insert asm6502-comment-char)
    (indent-for-comment))

   ;; Empty comment ends non-empty code line => new comment above.
   (t
    (goto-char comment)
    (skip-chars-backward " \t")
    (delete-region (point) (line-end-position))
    (beginning-of-line) (insert "\n") (backward-char)
    (asm6502-comment)))))

(provide 'asm6502-mode)

;;; asm6502-mode.el ends here

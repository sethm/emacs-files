(deftheme loomcom
  "High contrast theme with a traditional black background.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'loomcom
   `(default ((,class (:foreground "white" :background "black"))))))

(provide-theme 'loomcom)

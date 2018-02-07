(deftheme loomcom
  "High contrast theme with a traditional black background.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'loomcom
   `(default ((,class (:inherit nil
                                :stipple nil
                                :background "#333333"
                                :foreground "white"
                                :inverse-video nil
                                :box nil
                                :strike-through nil
                                :overline nil
                                :underline nil
                                :slant normal
                                :weight normal
                                :width normal
                                :family "Inconsolata"))))))

(provide-theme 'loomcom)

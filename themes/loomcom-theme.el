(deftheme loomcom
  "High contrast theme with a traditional black background.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'loomcom
   `(default ((,class (:inherit nil :stipple nil :background "black"
                                :foreground "white" :inverse-video nil
                                :box nil :strike-through nil
                                :overline nil :underline nil
                                :slant normal :weight normal
                                :height 140 :width normal
                                :foundry "Adobe"
                                :family "Source Code Pro"))))))

(provide-theme 'loomcom)

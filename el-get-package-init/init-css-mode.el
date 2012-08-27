;; CSS mode
(autoload 'css-mode "css-mode" nil t)
(add-hook 'css-mode-hook '(lambda ()
			    (setq css-indent-level 2)
			    (setq css-indent-offset 2)))

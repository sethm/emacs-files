((coffee-mode status "installed" recipe
	      (:name coffee-mode :website "http://ozmm.org/posts/coffee_mode.html" :description "Emacs Major Mode for CoffeeScript" :type github :pkgname "defunkt/coffee-mode" :features coffee-mode :post-init
		     (progn
		       (add-to-list 'auto-mode-alist
				    '("\\.coffee$" . coffee-mode))
		       (add-to-list 'auto-mode-alist
				    '("Cakefile" . coffee-mode))
		       (setq coffee-js-mode 'javascript-mode))))
 (haml-mode status "installed" recipe
	    (:name haml-mode :description "Major mode for editing Haml files" :type github :pkgname "nex3/haml-mode"))
 (inf-ruby status "installed" recipe
	   (:name inf-ruby :description "Inferior Ruby Mode" :features inf-ruby :type elpa))
 (magit status "installed" recipe
	(:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :info "." :autoloads
	       ("50magit")
	       :build
	       (("make" "all"))
	       :build/darwin
	       `(,(concat "make EMACS=" el-get-emacs " all"))))
 (package status "installed" recipe
	  (:name package :description "ELPA implementation (\"package.el\") from Emacs 24" :builtin 24 :type http :url "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el" :shallow nil :features package :post-init
		 (progn
		   (setq package-user-dir
			 (expand-file-name
			  (convert-standard-filename
			   (concat
			    (file-name-as-directory default-directory)
			    "elpa")))
			 package-directory-list
			 (list
			  (file-name-as-directory package-user-dir)
			  "/usr/share/emacs/site-lisp/elpa/"))
		   (make-directory package-user-dir t)
		   (unless
		       (boundp 'package-subdirectory-regexp)
		     (defconst package-subdirectory-regexp "^\\([^.].*\\)-\\([0-9]+\\(?:[.][0-9]+\\)*\\)$" "Regular expression matching the name of\n a package subdirectory. The first subexpression is the package\n name. The second subexpression is the version string."))
		   (setq package-archives
			 '(("ELPA" . "http://tromey.com/elpa/")
			   ("gnu" . "http://elpa.gnu.org/packages/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")
			   ("SC" . "http://joseito.republika.pl/sunrise-commander/"))))))
 (rhtml-mode status "installed" recipe
	     (:name rhtml-mode :description "Major mode for editing RHTML files" :type github :pkgname "eschulte/rhtml" :prepare
		    (progn
		      (autoload 'rhtml-mode "rhtml-mode" nil t)
		      (add-to-list 'auto-mode-alist
				   '("\\.html.erb$" . rhtml-mode)))))
 (ruby-compilation status "installed" recipe
		   (:name ruby-compilation :description "Run a ruby process in a compilation buffer" :type elpa))
 (ruby-mode status "installed" recipe
	    (:name ruby-mode :description "Major mode for editing Ruby files. RubyMode provides font-locking, indentation support, and navigation for Ruby code." :type elpa))
 (rvm status "installed" recipe
      (:name rvm :description "Emacs integration for rvm" :type github :features rvm :pkgname "senny/rvm.el" :compile "rvm.el" :post-init
	     (rvm-use-default)))
 (textmate status "installed" recipe
	   (:name textmate :description "TextMate minor mode for Emacs" :type github :pkgname "defunkt/textmate.el" :features textmate :post-init
		  (textmate-mode)))
 (yaml-mode status "installed" recipe
	    (:name yaml-mode :description "Simple major mode to edit YAML file for emacs" :type github :pkgname "yoshiki/yaml-mode" :prepare
		   (progn
		     (autoload 'yaml-mode "yaml-mode" nil t)
		     (add-to-list 'auto-mode-alist
				  '("\\.ya?ml\\'" . yaml-mode)))))
 (yasnippet status "installed" recipe
	    (:name yasnippet :website "https://github.com/capitaomorte/yasnippet.git" :description "YASnippet is a template system for Emacs." :type github :pkgname "capitaomorte/yasnippet" :features "yasnippet" :pre-init
		   (unless
		       (or
			(boundp 'yas/snippet-dirs)
			(get 'yas/snippet-dirs 'customized-value))
		     (setq yas/snippet-dirs
			   (list
			    (concat el-get-dir
				    (file-name-as-directory "yasnippet")
				    "snippets"))))
		   :post-init
		   (put 'yas/snippet-dirs 'standard-value
			(list
			 (list 'quote
			       (list
				(concat el-get-dir
					(file-name-as-directory "yasnippet")
					"snippets")))))
		   :compile nil :submodule nil)))

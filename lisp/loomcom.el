;;
;; Loom Communications Website Config
;;

(require 'ox-html)

(setq org-export-html-coding-system 'utf-8-unix
      org-html-viewport nil)

(setq loomcom/project-dir "~/Projects/loomcom/")

(setq loomcom/extra-head
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"/res/style.css\">")

(setq loomcom/header-file
      (concat loomcom/project-dir "pages/header.html"))

(setq loomcom/footer
      (concat
       "<div id=\"footer\">\n"
       "<p>Seth Morabito</p>\n"
       "<p>Proudly published with "
       "<a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> and "
       "<a href=\"https://orgmode.org/\">Org Mode</a>"
       "</div>"))

(setq org-html-html5-fancy t)

(defun loomcom/get-preview (filename)
  "Returns a list: '(<needs-more> <preview-string>) where
<needs-more> is t or nil, indicating whether a \"Read More...\"
link is needed."
  (with-temp-buffer
    (insert-file-contents (concat loomcom/project-dir "blog/" filename))
    (goto-char (point-min))
    (let ((content-start (or
                          ;; Look for the first non-keyword line
                          (and (re-search-forward "^[^#]" nil t)
                               (match-beginning 0))
                          ;; Failing that, assume we're malformed and
                          ;; have no content
                          (buffer-size)))
          (marker (or
                   (and (re-search-forward "^#\\+BEGIN_more$" nil t)
                        (match-beginning 0))
                   (buffer-size))))
      ;; Return a pair of '(needs-more preview-string)
      (list (not (= marker (buffer-size)))
            (buffer-substring content-start marker)))))

(defun loomcom/metadata (filename)
  "Get the Org-Mode metadata for a file"
  (with-temp-buffer
    (insert-file-contents (concat loomcom/project-dir "blog/" filename))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (element)
        (let ((key (org-element-property :key element))
              (value (org-element-property :value element)))
          `(,key ,value))))))

(defun loomcom/sitemap (title list)
  "Generate the sitemap (Blog Main Page)"
  (concat "#+TITLE: " title "\n" "--------\n"
          (string-join (mapcar #'car (cdr list)) "\n\n")))

(defun loomcom/sitemap-entry (entry style project)
  "Sitemap (Blog Main Page) Entry Formatter"
  (when (not (directory-name-p entry))
    (format (string-join
             '("* [[file:%s][%s]]\n"
               "  :PROPERTIES:\n"
               "  :RSS_PERMALINK: %s\n"
               "  :PUBDATE: %s\n"
               "  :END:\n"
               "#+BEGIN_published\n"
               "%s\n"
               "#+END_published\n"
               "%s\n"
               "--------\n"))
            entry
            (org-publish-find-title entry project)
            (concat (file-name-sans-extension entry) ".html")
            (format-time-string (car org-time-stamp-formats) (org-publish-find-date entry project))
            (format-time-string "%A, %B %_d %Y at %l:%M %p %Z" (org-publish-find-date entry project))
            (let* ((preview (loomcom/get-preview entry))
                   (needs-more (car preview))
                   (preview-text (cadr preview)))
              (if needs-more
                  (format
                   (concat
                    "%s\n\n"
                    "#+BEGIN_morelink\n"
                    "[[file:%s][Read More...]]\n"
                    "#+END_morelink\n")
                   preview-text entry)
                (format "%s" preview-text))))))

(defun loomcom/header (arg)
  (with-temp-buffer
    (insert-file-contents loomcom/header-file)
    (buffer-string)))

(setq org-publish-timestamp-directory (concat loomcom/project-dir "cache/"))

(setq org-publish-project-alist
      `(("loomcom"
         :components ("blog" "blog-rss" "pages" "res" "images"))
        ("blog"
         :base-directory ,(concat loomcom/project-dir "blog/")
         :base-extension "org"
         :publishing-directory ,(concat loomcom/project-dir "www/blog/")
         :publishing-function org-html-publish-to-html
         :with-author t
         :with-creator nil
         :with-date t
         :section-numbers nil
         :with-title t
         :with-toc nil
         :with-drawers t
         :with-sub-superscript nil
         :html-doctype "html5"
         :html-link-home "/"
         :html-head nil
         :html-head-extra ,loomcom/extra-head
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-viewport nil
         :html-link-up ""
         :html-link-home ""
         :html-preamble loomcom/header
         :html-postamble ,loomcom/footer
         :auto-sitemap t
         :sitemap-function loomcom/sitemap
         :sitemap-format-entry loomcom/sitemap-entry
         :sitemap-filename "index.org"
         :sitemap-title "A Weblog"
         :sitemap-sort-files anti-chronologically)
        ("blog-rss"
         :base-directory ,(concat loomcom/project-dir "blog/")
         :base-extension "org"
         :publishing-directory ,(concat loomcom/project-dir "www/blog/")
         :publishing-function org-rss-publish-to-rss
         :html-link-home "https://loomcom.com/blog/"
         :html-link-use-abs-url t
         :email "web@loomcom.com"
         :title "Loom Communications - Blog"
         :section-numbers nil
         :exclude ".*"
         :include ("index.org")
         :table-of-contents nil)
        ("pages"
         :base-directory ,(concat loomcom/project-dir "pages/")
         :base-extension "org"
         :publishing-directory ,(concat loomcom/project-dir "www/")
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :recursive t
         :with-title t
         :with-toc nil
         :with-drawers t
         :with-sub-superscript nil
         :html-link-home "/"
         :html-head nil
         :html-doctype "html5"
         :html-head-extra ,loomcom/extra-head
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-link-up ""
         :html-link-home ""
         :html-preamble loomcom/header
         :html-postamble ,loomcom/footer
         :html-viewport nil)
        ("res"
         :base-directory ,(concat loomcom/project-dir "res/")
         :base-extension ".*"
         :publishing-directory ,(concat loomcom/project-dir "www/res/")
         :publishing-function org-publish-attachment)
        ("images"
         :base-directory ,(concat loomcom/project-dir "images/")
         :base-extension ".*"
         :publishing-directory ,(concat loomcom/project-dir "www/images/")
         :publishing-function org-publish-attachment)))

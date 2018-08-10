;;
;; Loom Communications Website Config
;;

(require 'ox-html)

(setq org-export-html-coding-system 'utf-8-unix
      org-html-viewport nil
      org-html-html5-fancy t)

(setq lc/project-dir "~/Projects/loomcom/")

(setq lc/header-file
      (concat lc/project-dir "pages/header.html"))

(setq lc/extra-head
      (concat
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/res/style.css\">\n"
       "<link href=\"https://fonts.googleapis.com/css?family=Rubik\" rel=\"stylesheet\">\n"
       "<link href=\"https://fonts.googleapis.com/css?family=Source+Code+Pro\" rel=\"stylesheet\">\n"))

(setq lc/footer
      (concat
       "<div id=\"footer\">\n"
       "<p>Copyright 2018 by Seth Morabito. \n"
       "Proudly published with "
       "<a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> and "
       "<a href=\"https://orgmode.org/\">Org Mode</a>"
       "</div>"))


(defun lc/get-preview (filename)
  "Returns a list: '(<needs-more> <preview-string>) where
<needs-more> is t or nil, indicating whether a \"Read More →\"
link is needed."
  (with-temp-buffer
    (insert-file-contents (concat lc/project-dir "blog/" filename))
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

(defun lc/metadata (filename)
  "Get the Org-Mode metadata for a file"
  (with-temp-buffer
    (insert-file-contents (concat lc/project-dir "blog/" filename))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (element)
        (let ((key (org-element-property :key element))
              (value (org-element-property :value element)))
          `(,key ,value))))))

(defun lc/sitemap-for-group (title previous-page next-page list)
  "Generate the sitemap for one group of pages"
  (let ((previous-link (if previous-page
                           (format "[[%s][<< Previous Page]]" previous-page)
                         ""))
        (next-link (if next-page
                       (format "[[%s][Next Page >>]]" next-page)
                     "")))
    (concat "#+TITLE: " title "\n\n"
            "#+BEGIN_pagination\n"
            (format "- %s\n" previous-link)
            (format "- %s\n" next-link)
            "#+END_pagination\n\n"
            (string-join (mapcar #'car (cdr list)) "\n\n"))))

(defun lc/sitemap-entry (entry project lastp)
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
               "%s"))
            entry
            (org-publish-find-title entry project)
            (concat (file-name-sans-extension entry) ".html")
            (format-time-string (car org-time-stamp-formats) (org-publish-find-date entry project))
            (format-time-string "%A, %B %_d %Y at %l:%M %p %Z" (org-publish-find-date entry project))
            (let* ((preview (lc/get-preview entry))
                   (needs-more (car preview))
                   (preview-text (cadr preview)))
              (if needs-more
                  (format
                   (concat
                    "%s\n\n"
                    "#+BEGIN_morelink\n"
                    "[[file:%s][Read More →]]\n"
                    "#+END_morelink\n")
                   preview-text entry)
                (format "%s" preview-text)))
            (if lastp "" "--------\n"))))

(defun lc/header (arg)
  (with-temp-buffer
    (insert-file-contents lc/header-file)
    (buffer-string)))

(defun lc/sitemap-files-to-lisp (files project)
  "Convert a group of entries into a list"
  (let ((root (expand-file-name
               (file-name-as-directory
                (org-publish-property :base-directory project))))
        (last-item (car (reverse files))))
    (cons 'unordered
          (mapcar
           (lambda (f)
             (list (lc/sitemap-entry (file-relative-name f root) project (eq last-item f))))
           files))))

(defun lc/group (source n)
  "Group a list by 'n' elements"
  (if (not (endp (nthcdr n source)))
      (cons (subseq source 0 n)
            (lc/group (nthcdr n source) n))
    (list source)))

;;
;; This is a _heavily_ modified version of the original `org-publish-sitemap`
;; that is shipped with org-mode
;;
(defun org-publish-sitemap (project &optional sitemap-filename)
  "Publish the blog."
  (let* ((root (expand-file-name
		(file-name-as-directory
		 (org-publish-property :base-directory project))))
	 (title (or (org-publish-property :sitemap-title project)
		    (concat "Sitemap for project " (car project))))
         (sort-predicate
          (lambda (a b)
            (let* ((adate (org-publish-find-date a project))
                   (bdate (org-publish-find-date b project))
                   (A (+ (lsh (car adate) 16) (cadr adate)))
                   (B (+ (lsh (car bdate) 16) (cadr bdate))))
              (>= A B))))
         (file-filter (lambda (f) (not (string-match "index.*\\.org" f))))
         (files (seq-filter file-filter (org-publish-get-base-files project))))
    (message "Generating sitemap for Loomcom Weblog")
    (let* ((pages (sort files sort-predicate))
           (page-groups (lc/group pages 10))
           (page-number 0))
      (dolist (group page-groups page-number)
        (let ((fname (if (eq 0 page-number)
                         (concat root "index.org")
                       (format (concat root "index_%d.org") page-number)))
              (previous-page (cond ((eq 0 page-number) nil)
                                   ((eq 1 page-number) (concat root "index.org"))
                                   (t (format (concat root "index_%d.org") (- page-number 1)))))
              (next-page (if (eq (- (length page-groups) 1) page-number)
                             nil
                           (format (concat root "index_%d.org") (+ page-number 1)))))
          (setq page-number (+ 1 page-number))
          (with-temp-file fname
            (insert
             (lc/sitemap-for-group
              title
              previous-page
              next-page
              (lc/sitemap-files-to-lisp group project)))))))))

(setq org-publish-timestamp-directory (concat lc/project-dir "cache/"))

(setq org-publish-project-alist
      `(("loomcom"
         :components ("blog" "pages" "res" "images"))
        ("blog"
         :base-directory ,(concat lc/project-dir "blog/")
         :base-extension "org"
         :publishing-directory ,(concat lc/project-dir "www/blog/")
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
         :html-head-extra ,lc/extra-head
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-viewport nil
         :html-link-up ""
         :html-link-home ""
         :html-preamble lc/header
         :html-postamble ,lc/footer
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "A Weblog"
         :sitemap-sort-files anti-chronologically)
        ("pages"
         :base-directory ,(concat lc/project-dir "pages/")
         :base-extension "org"
         :publishing-directory ,(concat lc/project-dir "www/")
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
         :html-head-extra ,lc/extra-head
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-link-up ""
         :html-link-home ""
         :html-preamble lc/header
         :html-postamble ,lc/footer
         :html-viewport nil)
        ("res"
         :base-directory ,(concat lc/project-dir "res/")
         :base-extension ".*"
         :publishing-directory ,(concat lc/project-dir "www/res/")
         :publishing-function org-publish-attachment)
        ("images"
         :base-directory ,(concat lc/project-dir "images/")
         :base-extension ".*"
         :publishing-directory ,(concat lc/project-dir "www/images/")
         :publishing-function org-publish-attachment)))

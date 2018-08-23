;;
;; Loom Communications Website Config
;;

(require 'ox-html)

(setq org-export-html-coding-system 'utf-8-unix
      org-html-viewport nil
      org-html-html5-fancy t)

(setq loomcom-project-dir "~/Projects/loomcom/")

(setq loomcom-header-file
      (concat loomcom-project-dir "pages/header.html"))

(setq loomcom-extra-head
      (concat
       "<meta name=\"twitter:card\" content=\"summary\" />\n"
       "<meta name=\"twitter:site\" content=\"@twylo\" />\n"
       "<meta name=\"twitter:creator\" content=\"@twylo\" />\n"
       "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"https://loomcom.com/blog/index.xml\" />\n"
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/res/style.css\">\n"
       "<link href=\"https://fonts.googleapis.com/css?family=Rubik\" rel=\"stylesheet\">\n"
       "<link href=\"https://fonts.googleapis.com/css?family=Source+Code+Pro\" rel=\"stylesheet\">\n"))

(setq loomcom-footer
      (concat
       "<div id=\"footer\">\n"
       "<p>Copyright 2018 by Seth Morabito. \n"
       "Proudly published with "
       "<a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> and "
       "<a href=\"https://orgmode.org/\">Org Mode</a>"
       "</div>"))

(setq loomcom-posts-per-page 12)

(defun loomcom--get-preview (filename)
  "Returns a list: '(<needs-more> <preview-string>) where
<needs-more> is t or nil, indicating whether a \"Read More →\"
link is needed."
  (with-temp-buffer
    (insert-file-contents (concat loomcom-project-dir "blog/" filename))
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


(defun loomcom--sitemap-for-group (title previous-page next-page list)
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

;; Filter for 'pagination', 'published', and 'read more' blocks. This
;; feels like a gross kludge, and I'd like to find a better way to
;; handle this.
(defun loomcom--rss-special-block-filter (contents backend info)
  (if (or (string-match "<div class=\"pagination\">" contents)
          (string-match "<div class=\"published\">" contents)
          (string-match "<div class=\"morelink\">" contents))
      ""
    contents))

;; Create our special RSS backend to filter out special blocks.
(org-export-define-derived-backend 'loomcom-rss 'rss
  :filters-alist '((:filter-special-block . loomcom--rss-special-block-filter)))

;; Stolen directly from ox-rss.el, but 'rss changed to 'loomcom-rss.
;; This exists only so that we can filter out special blocks!
(defun org-rss-publish-to-loomcom-rss (plist filename pub-dir)
  "Publish an org file to RSS.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let ((bf (get-file-buffer filename)))
    (if bf
	  (with-current-buffer bf
	    (org-icalendar-create-uid filename 'warn-user)
	    (org-rss-add-pubdate-property)
	    (write-file filename))
      (find-file filename)
      (org-icalendar-create-uid filename 'warn-user)
      (org-rss-add-pubdate-property)
      (write-file filename) (kill-buffer)))
  (org-publish-org-to
   'loomcom-rss filename (concat "." org-rss-extension) plist pub-dir))

(defun loomcom--sitemap-entry (entry project lastp)
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
            (format-time-string (cdr org-time-stamp-formats) (org-publish-find-date entry project))
            (format-time-string "%A, %B %_d %Y at %l:%M %p %Z" (org-publish-find-date entry project))
            (let* ((preview (loomcom--get-preview entry))
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


(defun loomcom--header (arg)
  (with-temp-buffer
    (insert-file-contents loomcom-header-file)
    (buffer-string)))


(defun loomcom--sitemap-files-to-lisp (files project)
  "Convert a group of entries into a list"
  (let ((root (expand-file-name
               (file-name-as-directory
                (org-publish-property :base-directory project))))
        (last-item (car (reverse files))))
    (cons 'unordered
          (mapcar
           (lambda (f)
             (list (loomcom--sitemap-entry (file-relative-name f root) project (eq last-item f))))
           files))))


(defun loomcom--group (source n)
  "Group a list by 'n' elements"
  (if (not (endp (nthcdr n source)))
      (cons (subseq source 0 n)
            (loomcom--group (nthcdr n source) n))
    (list source)))


;;
;; This is a _heavily_ modified version of the original
;; `org-publish-sitemap` that is shipped with org-mode, changed to
;; support publishing multiple index pages.
;;
(defun org-publish-sitemap (project &optional sitemap-filename)
  "Publish the blog."
  (let* ((base (file-name-sans-extension (or sitemap-filename "index.org")))
         (root (expand-file-name
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
         (file-filter (lambda (f) (not (string-match (format "%s.*\\.org" base) f))))
         (files (seq-filter file-filter (org-publish-get-base-files project))))
    (message (format "Generating blog indexes for %s" title))
    (let* ((pages (sort files sort-predicate))
           (page-groups (loomcom--group pages loomcom-posts-per-page))
           (page-number 0))
      (dolist (group page-groups page-number)
        (let ((fname (if (eq 0 page-number)
                         (concat root (format "%s.org" base))
                       (concat root (format "%s_%d.org" base page-number))))
              (previous-page (cond ((eq 0 page-number) nil)
                                   ((eq 1 page-number) (concat root (format "%s.org" base)))
                                   (t (concat root (format "%s_%d.org" base (- page-number 1))))))
              (next-page (if (eq (- (length page-groups) 1) page-number)
                             nil
                           (concat root (format "%s_%d.org" base (+ page-number 1))))))
          (setq page-number (+ 1 page-number))
          (with-temp-file fname
            (insert
             (loomcom--sitemap-for-group
              title
              previous-page
              next-page
              (loomcom--sitemap-files-to-lisp group project)))))))))

(setq org-publish-timestamp-directory (concat loomcom-project-dir "cache/"))

(setq org-publish-project-alist
      `(("loomcom"
         :components ("blog" "blog-rss" "pages" "res" "images"))
        ("blog"
         :base-directory ,(concat loomcom-project-dir "blog/")
         :base-extension "org"
         :publishing-directory ,(concat loomcom-project-dir "www/blog/")
         :publishing-function org-html-publish-to-html
         :with-author t
         :author "Seth Morabito"
         :email "web@loomcom.com"
         :with-creator nil
         :with-date t
         :section-numbers nil
         :with-title t
         :with-toc nil
         :with-drawers t
         :with-sub-superscript nil
         :html-doctype "html5"
         :html-link-home "https://loomcom.com/"
         :html-link-use-abs-url t
         :html-head nil
         :html-head-extra ,loomcom-extra-head
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-viewport nil
         :html-link-up ""
         :html-link-home ""
         :html-preamble loomcom--header
         :html-postamble ,loomcom-footer
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Seth Morabito • A Weblog"
         :sitemap-sort-files anti-chronologically)
        ("blog-rss"
         :base-directory ,(concat loomcom-project-dir "blog/")
         :base-extension "org"
         :rss-image-url "https://loomcom.com/images/loomcom_logo_sm.png"
         :rss-link-home "https://loomcom.com/blog/"
         :html-link-home "https://loomcom.com/blog/"
         :html-link-use-abs-url t
         :author "Seth Morabito"
         :email "web@loomcom.com"
         :rss-extension "xml"
         :publishing-directory ,(concat loomcom-project-dir "www/blog/")
         :publishing-function (org-rss-publish-to-loomcom-rss)
         :section-numbers nil
         :exclude ".*"
         :include ("index.org")
         :table-of-contents nil)
        ("pages"
         :base-directory ,(concat loomcom-project-dir "pages/")
         :base-extension "org"
         :publishing-directory ,(concat loomcom-project-dir "www/")
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
         :html-head-extra ,loomcom-extra-head
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-link-up ""
         :html-link-home ""
         :html-preamble loomcom--header
         :html-postamble ,loomcom-footer
         :html-viewport nil)
        ("res"
         :base-directory ,(concat loomcom-project-dir "res/")
         :base-extension ".*"
         :publishing-directory ,(concat loomcom-project-dir "www/res/")
         :publishing-function org-publish-attachment)
        ("images"
         :base-directory ,(concat loomcom-project-dir "images/")
         :base-extension ".*"
         :publishing-directory ,(concat loomcom-project-dir "www/images/")
         :publishing-function org-publish-attachment)))

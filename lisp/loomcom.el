;;; loomcom.el --- Loom Communications Website Publishing
;;;
;;; Commentary:
;;;

(require 'org)
(require 'ox-html)

;;; Code:

(defvar loomcom-project-dir)
(defvar loomcom-org-dir)
(defvar loomcom-www-dir)
(defvar loomcom-blog-org-dir)
(defvar loomcom-blog-www-dir)
(defvar loomcom-header-file)
(defvar loomcom-head)
(defvar loomcom-footer)
(defvar loomcom-posts-per-page)

(setq loomcom-project-dir "~/Projects/loomcom/")
(setq loomcom-org-dir (concat loomcom-project-dir "org/"))
(setq loomcom-www-dir (concat loomcom-project-dir "www/"))
(setq loomcom-blog-org-dir (concat loomcom-org-dir "blog/"))
(setq loomcom-blog-www-dir (concat loomcom-www-dir "blog/"))

(setq loomcom-header-file
      (concat loomcom-project-dir "org/header.html"))

(setq loomcom-head
      (concat
       "<meta name=\"twitter:site\" content=\"@twylo\" />\n"
       "<meta name=\"twitter:creator\" content=\"@twylo\" />\n"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />\n"
       "<link rel=\"icon\" type=\"image/png\" href=\"/images/icon/favicon-32x32.png\" />\n"
       "<link rel=\"apple-touch-icon-precomposed\" href=\"/images/icon/apple-touch-icon.png\" />\n"
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/res/faces.css\">\n"
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/res/style.css\">\n"))

(setq loomcom-footer
      (concat
       "<div id=\"footer\">\n"
       "<p>Copyright 2019 by Seth Morabito. \n"
       "Proudly published with "
       "<a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> and "
       "<a href=\"https://orgmode.org/\">Org Mode</a>"
       "</div>"))

(setq loomcom-posts-per-page 12)

(defun loomcom--get-preview (filename)
  "Get a preview string for a file.
This function returns a list, '(<needs-more> <preview-string>),
where <needs-more> is nil or non-nil, and indicates whether
a \"Read More →\" link is needed.

FILENAME The file to get a preview for."
  (with-temp-buffer
    (insert-file-contents (concat loomcom-blog-org-dir filename))
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
  "Generate the sitemap for one group of pages.

TITLE  The title of the page
PREVIOUS-PAGE  The previous index page to link to.
NEXT-PAGE  The next index page to link to.
LIST  The group of pages."
  (let ((previous-link (if previous-page
                           (format "[[%s][← Previous Page]]" previous-page)
                         ""))
        (next-link (if next-page
                       (format "[[%s][Next Page →]]" next-page)
                     "")))
    (concat "#+TITLE: " title "\n\n"
            "#+BEGIN_pagination\n"
            (format "- %s\n" previous-link)
            (format "- %s\n" next-link)
            "#+END_pagination\n\n"
            (string-join (mapcar #'car (cdr list)) "\n\n"))))

(defun loomcom--sitemap-entry (entry project)
  "Sitemap (Blog Main Page) Entry Formatter.

ENTRY  The sitemap entry to format.
PROJECT  The project structure."
  (when (not (directory-name-p entry))
    (format (string-join
             '("* [[file:%s][%s]]\n"
               "  :PROPERTIES:\n"
               "  :PUBDATE: %s\n"
               "  :END:\n"
               "#+BEGIN_published\n"
               "%s\n"
               "#+END_published\n"
               "%s"))
            entry
            (org-publish-find-title entry project)
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
                (format "%s" preview-text))))))

(defun loomcom--header (_)
  "Insert the header of the page."
  (with-temp-buffer
    (insert-file-contents loomcom-header-file)
    (buffer-string)))

(defun loomcom--sitemap-files-to-lisp (files project)
  "Convert a group of entries into a list.

FILES  The group of entries to list-ify.
PROJECT  The project structure."
  (let ((root (expand-file-name
               (file-name-as-directory
                (org-publish-property :base-directory project)))))
    (cons 'unordered
          (mapcar
           (lambda (f)
             (list (loomcom--sitemap-entry (file-relative-name f root) project)))
           files))))

(defun loomcom--group (source n)
  "Group a list by 'n' elements.

SOURCE  The list.
N  The number to group the list by."
  (if (not (endp (nthcdr n source)))
      (cons (subseq source 0 n)
            (loomcom--group (nthcdr n source) n))
    (list source)))

;;
;; We keep a local cache of filename to date. This speeds up
;; publishing tremendously, because org-publish-find-date is very
;; expensive, and the sorting predicate we use calls it n^2 times.
;;
(setq loomcom-sitemap-file-dates (make-hash-table))

(defun loomcom--find-date (file-name project)
  "Find the date for a file and cache it.

FILE-NAME  The file in which to find a date.
PROJECT  The project structure."
  (let ((maybe-date (gethash file-name loomcom-sitemap-file-dates nil)))
    (if maybe-date
        maybe-date
      (let ((new-date (org-publish-find-date file-name project)))
        (puthash file-name new-date loomcom-sitemap-file-dates)
        new-date))))

;; Un-define the original version of 'org-publish-sitemap'
(fmakunbound 'org-publish-sitemap)

;; Define our own version.
(defun org-publish-sitemap (project &optional sitemap-filename)
  "Publish the blog.

This is actually a heavily modified and customized version of the
function by the same name in ox-publish.el.  It allows the
generation of a sitemap with multiple pages.

PROJECT  The project structure.
SITEMAP-FILENAME  The filename to use as the default index."
  (let* ((base (file-name-sans-extension (or sitemap-filename "index.org")))
         (root (file-name-as-directory (expand-file-name
                                        (concat loomcom-org-dir "blog/"))))
	 (title (or (org-publish-property :sitemap-title project)
		    (concat "Sitemap for project " (car project))))
         (sort-predicate
          (lambda (a b)
            (let* ((adate (loomcom--find-date a project))
                   (bdate (loomcom--find-date b project))
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
         :components ("blog" "pages" "res" "images"))

        ("blog"
         :base-directory ,loomcom-blog-org-dir
         :base-extension "org"
         :publishing-directory ,loomcom-blog-www-dir
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
         :html-head ,loomcom-head
         :html-head-extra nil
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-viewport nil
         :html-link-up ""
         :html-link-home ""
         :html-preamble loomcom--header
         :html-postamble ,loomcom-footer
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Seth Morabito ∴ A Weblog"
         :sitemap-sort-files anti-chronologically)

        ("pages"
         :base-directory ,loomcom-org-dir
         :base-extension "org"
         :exclude ".*blog/.*"
         :publishing-directory ,loomcom-www-dir
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :recursive t
         :with-title t
         :with-toc nil
         :with-drawers t
         :with-sub-superscript nil
         :with-author t
         :author "Seth Morabito"
         :email "web@loomcom.com"
         :with-creator nil
         :with-date t
         :html-link-home "/"
         :html-head nil
         :html-doctype "html5"
         :html-head ,loomcom-head
         :html-head-extra nil
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-link-up ""
         :html-link-home ""
         :html-preamble loomcom--header
         :html-postamble ,loomcom-footer
         :html-viewport nil)

        ("res"
         :base-directory ,loomcom-org-dir
         :base-extension "css\\|js\\|woff2\\|woff\\|ttf"
         :recursive t
         :publishing-directory ,loomcom-www-dir
         :publishing-function org-publish-attachment)

        ("images"
         :base-directory ,loomcom-org-dir
         :base-extension "png\\|jpg\\|gif\\|pdf"
         :recursive t
         :publishing-directory ,loomcom-www-dir
         :publishing-function org-publish-attachment)))

(provide 'loomcom)
;;; loomcom.el ends here

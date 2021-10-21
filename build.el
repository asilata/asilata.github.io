(message (emacs-version))
(require 'package)
(package-initialize)
(unless package-archive-contents
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (package-refresh-contents))
(dolist (pkg '(org))
  (unless (package-installed-p pkg)
    (package-install pkg)))
(require 'org)
(require 'ox-publish)
(require 'subr-x)

(setq org-publish-project-alist
        '(("website"
           :base-directory "./content"
           :publishing-directory "./public"
           :recursive t
           :exclude "data\\|stylesheet"
           :publishing-function org-html-publish-to-html
           :with-creator t
           :with-title nil
           :with-toc nil
           :html-preamble make-header
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           :html-head "<link rel=\"stylesheet\" href=\"css/stylesheet.css\">"
           :html-head-extra "<link href=\"https://fonts.googleapis.com/css?family=Neuton:400,400i,700,700i\" rel=\"stylesheet\"><script>
   (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
       (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
   })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
   ga('create', '{{ site.analytics_id}}', 'auto');
   ga('send', 'pageview');
  </script>"
           :section-numbers nil
           )
          ("assets"
           :base-directory "./content/assets"
           :base-extension "jpg\\|pdf"
           :publishing-directory "./public/assets"
           :recursive t
           :publishing-function org-publish-attachment)
          ("css"
           :base-directory "./content/css"
           :base-extension "css"
           :publishing-directory "./public/css"
           :recursive t
           :publishing-function org-publish-attachment)
          ))

(setq org-html-postamble t
      org-html-postamble-format '(("en" "<footer>
Page generated using %c using the <a href=\"https://ethanschoonover.com/solarized/\">solarized</a> colour theme. Last modified on %C. Source on <a href=\"https://github.com/asilata/asilata.github.io\">github</a>.
</footer>")))

(defun make-header (options)
  (concat "<header>"
          "<h1><a href=\"/\">Asilata Bapat</a></h1>"
          "<nav>"
          (make-navbar)
          "</nav></header>"))

(defun make-navbar ()
  (let ((lst '("Home" "Teaching" "Research" "Activities"))
        (title (cadar (org-collect-keywords '("TITLE")))))
    (concat "<ul>\n"
            (string-join (mapcar (lambda (x) (make-nav-item x title)) lst) "\n")
            "\n</ul>")))

(defun make-nav-item (str title)
  (let ((slug (if (string-equal str "Home") "index" (downcase str))))
    (format "<li><a href=\"%s.html\"%s>%s</a></li>"
            slug
            (if (string-equal str title) " class=\"active\"" "")
            str)))

(defun pp-course ()
  "Pretty-print the current course item. This function is called when mapping over entries in the data.org file."
  (let ((title (org-entry-get nil "ITEM"))
        (display-date (org-entry-get nil "display-date"))
        (comment (org-entry-get nil "comment"))
        )
    (format "- **%s**: %s %s"
            display-date
            title
            (if (or (not comment) (string-equal comment ""))
                ""
              comment))
    ))

(defun pp-research-output ()
  "Pretty-print the current research output item (e.g. paper, preprint, or code). This function is called when mapping over entries in the data.org file."
   (let ((title (org-entry-get nil "ITEM"))
         (with (org-entry-get nil "with"))
         (comment (org-entry-get nil "comment"))
         (links (org-entry-get-multivalued-property nil "link")))
     (format "#+begin_papers\n**%s**%s\\\\\n%s\n\n%s\n#+end_papers"
             title
             (if (or (not with) (string-equal with ""))
                 ""
               (format " (with %s)" with))
             comment
             (string-join (mapcar (lambda (l) (format "[%s]" l)) links) " "))
     ))

(defun pp-activity ()
  "Pretty-print the current activity item. This function is called when mapping over entries in the data.org file."
  (let ((title (org-entry-get nil "ITEM"))
        (scheduled (encode-time (org-parse-time-string (org-entry-get nil "SCHEDULED"))))
        (display-date (org-entry-get nil "display-date"))
        (with (org-entry-get nil "with"))
        (location (org-entry-get nil "location"))
        (links (org-entry-get-multivalued-property nil "link"))
        (comment (org-entry-get nil "comment")))
    (if links (message (car links)))
    (format "- **%s**: %s%s%s%s%s"
            (if display-date display-date
              (org-format-time-string "%b %Y" scheduled))
            title
            (if location (concat ", " location) "")
            (if with (concat ", with " with) "")
            (if comment (format " (%s)" comment) "")
            (if links
                (concat " "
                        (string-join
                         (mapcar (lambda (l) (format "[%s]" l)) links) " "))
              ""))))

(setq org-confirm-babel-evaluate nil)
(setq make-backup-files nil)
(org-publish-all t)
(message "Build complete!")

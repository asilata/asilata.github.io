(message (emacs-version))
(require 'ox-publish)
(require 'subr-x)

(let ((ab/html-head "<link rel=\"stylesheet\" href=\"css/stylesheet.css\">")
      (ab/html-head-extra
       (string-join 
        '("<link href=\"https://fonts.googleapis.com/css?family=Neuton:400,400i,700,700i\" rel=\"stylesheet\">"
          "<script>
     (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
         (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
     })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
     ga('create', '{{ site.analytics_id}}', 'auto');
     ga('send', 'pageview');
    </script>")))
      (ab/html-postamble-format
       '(("en" "<footer>
    Page generated using %c. Last modified on %C. Source on <a href=\"https://github.com/asilata/asilata.github.io\">github</a>.
    </footer>")))
      (ab/latex-classes
       '(("moderncv"
          "\\documentclass[a4paper]{moderncv}
          [NO-DEFAULT-PACKAGES]
          \\usepackage{amsmath,amssymb}"
          ("\\section{%s}" . "\\section*{%s}")
          ("\\subsection{%s}" . "\\subsection*{%s}")
          ("\\paragraph{%s}" . "\\paragraph*{%s}")
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
      )

(setq org-publish-project-alist
      `(("website"
         :base-directory "./content"
         :publishing-directory "./public"
         :recursive t
         :exclude "data\\|cv\\|stylesheet"
         :publishing-function org-html-publish-to-html
         :with-broken-links t
         :with-creator t
         :with-title nil
         :with-toc nil
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-head ,ab/html-head
         :html-head-extra ,ab/html-head-extra
         :html-postamble-format ,ab/html-postamble-format
         :html-preamble ab/make-html-preamble
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
        ("cv"
         :base-directory "./content/cv"
         :publishing-directory "./public/cv"
         :latex-hyperref-template nil
         :latex-classes ,ab/latex-classes
         :publishing-function org-latex-publish-to-latex
         )
        )
      ))

(defun ab/make-html-preamble (options)
  (concat "<header>"
          "<h1><a href=\"/\">Asilata Bapat</a></h1>"
          "<nav>"
          (ab/make-navbar)
          "</nav></header>"))

(defun ab/make-navbar ()
  (let ((lst '("Home" "Teaching" "Research" "Activities"))
        ;; (title (cadar (org-collect-keywords '("TITLE"))))
        )
    (concat "<ul>\n"
            (string-join
             (mapcar (lambda (x) (ab/make-nav-item x)) lst) "\n")
            "\n</ul>")))

(defun ab/make-nav-item (str)
  (let ((slug (if (string-equal str "Home") "index" (downcase str))))
    (format "<li><a href=\"%s.html\">%s</a></li>"
            slug
            str)))  
(defun ab/make-nav-item-fancy (str title)
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
    (format "- *%s*: %s %s"
            display-date
            title
            (if (or (not comment) (string-equal comment ""))
                ""
              comment))
    ))

(defun pp-short-talk ()
  (let ((shorttitle (org-entry-get nil "shorttitle"))
        (date (encode-time (org-parse-time-string (org-entry-get nil "SCHEDULED"))))
        (location (org-entry-get nil "location"))
        (links (org-entry-get nil "link")))
    (format "%s%s"
            (concat
             (if shorttitle (concat shorttitle " ")
               (if location (concat location " ") ""))
             (org-format-time-string "%Y" date))
            (if links (format " (%s)" links) ""))))

(defun paper-to-talks (slug)
  (string-join
   (org-map-entries
    'pp-short-talk
    (format "activity+%s" slug)
    '("data.org"))
   ", "))

(defun pp-research-output ()
  "Pretty-print the current research output item (e.g. paper, preprint, or code). This function is called when mapping over entries in the data.org file."
  (let ((title (org-entry-get nil "ITEM"))
        (with (org-entry-get nil "with"))
        (comment (org-entry-get nil "comment"))
        (links (org-entry-get nil "link"))
        (slug (org-entry-get nil "slug")))
    (format "#+begin_papers\n*%s*%s%s%s\n\n%s\n#+end_papers"
            title
            (if (or (not with) (string-equal with ""))
                ""
              (format " (with %s)" with))
            (if comment (concat "\\\\\n" comment) "")
            (if slug (concat "\\\\\n*Talks:* "(paper-to-talks slug)) "")
            (if links (format "(%s)" links) ""))))

(defun pp-activity ()
  "Pretty-print the current activity item. This function is called when mapping over entries in the data.org file."
  (let ((title (org-entry-get nil "ITEM"))
        (scheduled (encode-time (org-parse-time-string (org-entry-get nil "SCHEDULED"))))
        (display-date (org-entry-get nil "display-date"))
        (with (org-entry-get nil "with"))
        (location (org-entry-get nil "location"))
        (links (org-entry-get nil "link"))
        (comment (org-entry-get nil "comment")))
    (format "- *%s*: %s%s%s%s%s"
            (if display-date display-date
              (org-format-time-string "%b %Y" scheduled))
            title
            (if location (concat ", " location) "")
            (if with (concat ", with " with) "")
            (if comment (format " (%s)" comment) "")
            (if links (format " (%s)" links) ""))
            ))

(setq org-confirm-babel-evaluate nil)
(setq make-backup-files nil)
(org-publish-all t)
(message "Build complete!")

(message (emacs-version))
(require 'ox-publish)
(require 'subr-x)

(let ((ab/html-head "<link rel=\"stylesheet\" href=\"css/stylesheet.css\">")
      (ab/html-head-extra
       (string-join 
        '("<link href=\"https://fonts.googleapis.com/css?family=Neuton:400,400i,700,700i\" rel=\"stylesheet\">")))
      (ab/html-postamble
       "<footer>
    Page generated using %c. Last modified on %C. Source on <a href=\"https://github.com/asilata/asilata.github.io\">github</a>.
    </footer>")
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
         :html-postamble ,ab/html-postamble
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
        ("cv-pdf"
         :base-directory "./content/cv"
         :publishing-directory "./public/cv"
         :latex-hyperref-template nil
         :latex-classes ,ab/latex-classes
         :publishing-function org-latex-publish-to-latex
         )
        ("cv-html"
         :base-directory "./content/cv"
         :publishing-directory "./public"
         :publishing-function org-html-publish-to-html
         :with-broken-links t
         :with-creator t
         :with-title nil
         :with-toc nil
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-head ,ab/html-head
         :html-head-extra ,ab/html-head-extra
         :html-postamble ,ab/html-postamble
         :html-preamble ab/make-html-preamble
         :section-numbers nil
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
        (title (cadar (org-collect-keywords '("TITLE")))))
    (concat "<ul>\n"
            (string-join
             (mapcar (lambda (x) (ab/make-nav-item x title)) lst) "\n")
            "\n</ul>")))

(defun ab/make-nav-item (str title)
  (let ((slug (if (string-equal str "Home") "index" (downcase str))))
    (format "<li><a href=\"%s.html\"%s>%s</a></li>"
            slug
            (if (string-equal str title) " class=\"active\"" "")
            str)))

(setq org-confirm-babel-evaluate nil)
(setq make-backup-files nil)
(org-publish-all t)
(message "Build complete!")

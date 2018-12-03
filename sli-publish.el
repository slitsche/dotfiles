
;; the variable org-html-metadata-timestamp-format
;; is not used for %d in org version 8.2.10
;; (setq org-export-date-timestamp-format "%Y-%m-%d")


;; <link rel=\"apple-touch-icon-precomposed\" href=\"static/favicon-152.png\">
;; <link rel=\"msapplication-TitleImage\" href=\"static/favicon-144.png\">
;; <link rel=\"msapplication-TitleColor\" href=\"#0141ff\">
;; <script src=\"static/katex.min.js\"></script>
;; <script src=\"static/auto-render.min.js\"></script>
;; <link rel=\"stylesheet\" href=\"static/katex.min.css\">
;; <script>document.addEventListener (\"DOMContentLoaded\", function() { renderMathInElement(document.body); });</script>

(setq org-static-blog-page-header
"<meta name=\"author\" content=\"Stefan Litsche\">
<meta name=\"referrer\" content=\"no-referrer\">
<link href= \"css/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"img/favicon.ico\">
<meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\">
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">")

(setq sli-biosoft-header
      (concat "<div id=\"header\">"
       "<h1>Stefan Litsche</h1>"
       "<ul>"
           "<li><a href=\"/\">Home</a></li>"
;;           "<li><a href=\"/articles.html\">Blog</a></li>"
;;           "<li><a href=\"/projects/\">Projects</a></li>"
           "<li><a href=\"/impressum.html\">Impressum</a></li>"
           "<li><a href=\"/rss.xml\">RSS</a></li>"
       "</ul>"
       "</div>"))

(setq sli-biosoft-footer
      (concat "<p>"
              "Created: %d "
              "Last modified: %C "
              "Handcrafted using Emacs and Orgmode"
              "</p>"))


;;; org-static-blog
(setq org-static-blog-publish-title "biosoft.de")
(setq org-static-blog-publish-url "https://biosoft.de/")
(setq org-static-blog-publish-directory "~/work/git/biosoft/html/")
(setq org-static-blog-posts-directory "~/work/git/biosoft/src/")
(setq org-static-blog-drafts-directory "~/work/git/biosoft/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-static-blog-page-preamble sli-biosoft-header)
(setq org-static-blog-page-postamble sli-biosoft-footer)

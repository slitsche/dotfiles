;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

(require 'ox-publish)
(setq org-publish-project-alist
      '(

        ;; ... add allthe components here (see below)...
        ("org-notes"
         :base-directory "~/work/git/biosoft/src/"
         :base-extension "org"
         :publishing-directory "~/work/git/biosoft/html/"
         :html-head-include-default-style nil
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/style.css\">"
         :recusive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )

        ("org-static"
        :base-directory "~/work/git/biosoft/src/"
        :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
        :publishing-directory "~/work/git/biosoft/html/"
        :recursive t
        :publishing-function org-publish-attachment
        )

        ("biosoft" :components ("org-notes" "org-static"))
        ))
(setq org-html-html5-fancy t)

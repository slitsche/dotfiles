
;;; ============ ORG ============
(require 'org)
(require 'org-bullets)
(require 'org-id)
(require 'org-datetree)
(require 'ob-clojure)
(require 'org-ql)
(require 'org-ql-view)
;; (require 'helm-org-ql)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)")
        (type  "MEET")))

;; The following is important to be able to get my worklog from
;; various files and nodes using agenda and log-mode
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-catch-invisible-edits 'show-and-error)
(setq org-startup-indented t)

;; Setup folder structure
(setq org-directory "~/Documents/org")
(setq org-agenda-files (list "~/Documents/org/gtd.org"
                             "~/Documents/org/todo.org"
                             "~/Documents/org/privat.org"
                             "~/Documents/org/inbox.org"
                             "~/Documents/org/networking.org"))

;; in emacs 26 changing it requires restart
(setq sli-notes-files '("~/Documents/org/notes.org"
                        "~/Documents/org/emacs.org"
                        "~/Documents/org/projects.org"
                        "~/Documents/org/someday.org"
                        "~/Documents/org/notes/customer-inbox.org"
                        ;; "~/Documents/org/notes/cassandra-training.org"
                        ;; "~/Documents/org/notes/segeln.org"
                        ))

;; My daily work agenda
(defun sli-work-agenda ()
  (interactive)
  (org-agenda nil "w"))

;; Define a set of different files
(setq sli-work-agenda
      (seq-remove (lambda (x) (string-match "privat" x))
                  org-agenda-files))

(setq org-agenda-text-search-extra-files sli-notes-files)

;; I want to see the ancestors when I jump from agenda into tree
(setq org-show-context-detail
      '((agenda . ancestors)
        (bookmark-jump . lineage)
        (isearch . lineage)
        (default . ancestors)))

;; In order to organize references we add notes to the refile targets
;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))
                           (sli-notes-files . (:maxlevel . 3))
                           (("~/Documents/org/someday.org") . (:maxlevel . 3))))

;; https://dindi.garjola.net/zettelkustom.html
(defun sli-zettel-template ()
  "* %?\n:PROPERTIES:\n:DATE_CREATED: %U\n:FROM: %a\n:END:\n%i\n")

;; if we store a link we want to generate an id-property
(setq org-id-link-to-org-use-id t)

;; inspired by https://dindi.garjola.net/zettelkustom.html
(defun sli-zettel-backlinks ()
  (interactive)
  (let* ((id (org-entry-get (point) "ID"))
         (query (cond (id `(link :target ,(concat "id:" id )))
                      (t (error "Entry has no ID property"))))
         ;; (helm-query (concat "link:target=" (concat "\"" "id:" id "\"")))
         (title (concat "Links to: " (org-get-heading t t)))
         (org-agenda-tag-filter nil))
    ;; (message helm-query)
    ;; (helm-org-ql sli-notes-files :name helm-query)
    (org-ql-search (append sli-work-agenda sli-notes-files) query :title title)))

;https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-capture-templates
      '(("t" "Todo task inbox" entry
         (file "inbox.org")
         "* TODO %?\n%U"
         :empty-lines-before 1)
        ("r" "read" entry
         (file "inbox.org")
         "* %? :READ:\n%U\n")
        ("n" "note" entry
         (file "inbox.org")
         "* %?\n%U"
         :empty-lines-before 1)
        ("f" "folgezettel" entry
         (file "inbox.org")
         (function sli-zettel-template)
         :empty-lines-before 1)
        ("o" "Orga" entry
         (file+headline "gtd.org" "Orga Stuff")
         "* DONE %^{Title}\nCLOSED: %U\n%?")
        ("c" "Consulting" entry
         (file+headline "gtd.org" "Consulting")
         "* MEET %^{Title}\nCLOSED: %U\n%?"
         :clock-in t
         :empty-lines-before 1)
        ("m" "Meeting" entry
         (file "inbox.org")
         "* MEET %^{Title}\nCLOSED: %U\n%?"
         :clock-in t
         :empty-lines-before 1)
        ("j" "Journal" entry
         (file "todo.org")
         "* DONE %?\nCLOSED: %U"
         :empty-lines-before 1)
        ("e" "Event log" entry
         (file+datetree "notes/eventlog.org")
         "*** %?\n%U")
        ("h" "Health" entry
         (file+datetree "no-agenda/health.org")
         "*** %?\n%U")
        ("s" "Segeln" entry
         (file+headline "notes/segeln.org" "Zettels")
         (function sli-zettel-template)
         :empty-lines 1)))

;; http://doc.norang.ca/org-mode.html#CustomAgendaViews
;; https://emacs.stackexchange.com/questions/12517/how-do-i-make-the-timespan-shown-by-org-agenda-start-yesterday

(setq org-agenda-custom-commands
      (quote (("n" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("r" "Review"
               ((todo "TODO"
                      ((org-agenda-files '("~/Documents/org/inbox.org"))
                       (org-agenda-overriding-header "New Items")))
                (tags-todo "-Boss/!TODO"
                           ((org-agenda-overriding-header "Next Todos")
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)))
                (tags-todo "+READ/!TODO"
                           ((org-agenda-overriding-header "Next Read")
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-files (append org-agenda-files sli-notes-files))))))
              ("w" "My Work List"
               ((agenda ""
                        ((org-agenda-span 'day)
                         (org-agenda-files sli-work-agenda)))
                (tags-todo "-Boss-READ/!NEXT"
                           ((org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-files (append sli-work-agenda sli-notes-files)))
                           nil)
                (tags-todo "READ/!NEXT"
                           ((org-agenda-files (append org-agenda-files sli-notes-files))
                            (org-agenda-overriding-header "Reading List")))))
              ("d" "Delegated"
               (;;;(agenda "" ((org-agenda-span 'week)))
                (todo "WAIT"
                           ((org-agenda-files sli-work-agenda)))))
              ("p" "Private stuff"
               ( ;;;(agenda "" ((org-agenda-span 'week)))
                (tags-todo "-READ/TODO"
                           ((org-agenda-files '("~/Documents/org/privat.org"))))))
              ("U" tags-tree "+Boss"
               ((org-show-context-detail 'minimal))))))

(setq org-tag-alist '(
                      ("Alex" . ?a)
                      ("Boss" . ?b)
                      ("Claus" . ?c)
                      ("petra" . ?d)
                      ("PE" . ?e)
                      ("Qst" . ?f)
                      ("Goal" . ?g)
                      ("hardik" . ?h)
                      ("Geld" . ?m)
                      ("nuntius" . ?n)
                      ("ORGA" . ?o)
                      ("Proj" . ?p)
                      ("READ" . ?r)
                      ("Sergio" . ?s)
                      ("Waleed" . ?w)
                      ))

(setq org-tags-exclude-from-inheritance '("NOTE" "Proj"))

(setq org-sort-agenda-noeffort-is-high nil)
(setq org-global-properties
      '(("Effort_ALL". "0:10 0:30 1:00 2:00")))

(setq org-stuck-projects
      '("+Proj/-DONE" ("NEXT" "WAIT") nil nil))
(add-to-list 'org-structure-template-alist
             '("C" "#+BEGIN_COMMENT\n?\n#+END_COMMENT" "<!--?-->"))

;;https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
;; Make it possible to refile with helm
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; I want to clean up my todo file which is a long list of TODOs and DONE in between
;; https://stackoverflow.com/a/27043756/4096511
;; the last parameter can be 'tree or 'agenda
(defun sli-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(defun sli-report-worklog ()
  "Report on all consulting actions from worklog"
  (interactive)
  (message
   (format "Consultings: %d"
           (length
            (org-map-entries t "CONS" 'tree)))))

(defun sli-read-datetree-date (d)
  "Parse a time string D and return a date to pass to the datetree functions."
  (let ((dtmp (nthcdr 3 (parse-time-string d))))
    (list (cadr dtmp) (car dtmp) (caddr dtmp))))

(defun sli-refile-to-archive-datetree (&optional bfn)
  "Refile an entry to a datetree under an archive."
  (interactive)
  (let* ((bfn    (or bfn
                     (find-file-noselect
                      (expand-file-name "~/Documents/org/no-agenda/worklog.org"))))
         (datetree-date (sli-read-datetree-date (org-read-date t nil)))
         (target-pos  (with-current-buffer bfn
                                (save-excursion
                                  (org-datetree-find-date-create datetree-date)
                                  (point)))))
    (org-refile nil nil (list nil (buffer-file-name bfn) nil target-pos)))
  (setq this-command 'my/org-refile-to-journal))

;; Setup Org Babel
(setq org-babel-clojure-backend 'cider)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   ;;(sh . t)
   (emacs-lisp . t)
   (clojure . t)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "<f12>") 'sli-work-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
 ;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; https://www.masteringemacs.org/article/what-is-new-in-emacs-24-part-2
;(setq package-enable-at-startup nil)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/evil-magit")
(require 'evil)
(package-initialize)

(ensure-package-installed
                          'cider
                          'clj-refactor
                          'clojure-mode
                          'company
                          ;'evil
                          ;'evil-magit
                          'elfeed
                          'fill-column-indicator
                          'go-autocomplete
                          'go-eldoc
                          'go-mode
                          'helm
                          'helm-projectile
                          ;; for html export from org-mode
                          'htmlize
                          'ibuffer
                          'magit
                          ;'magit-gh-pulls
                          'markdown-mode
                          'org-bullets
                          'paredit
                          'projectile
                          'slime
                          'use-package
                          'yaml-mode)

;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))

;; http://ergoemacs.org/emacs/organize_your_dot_emacs.html
(defun xah-get-fullpath (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's file location.

Example: If you have this line
 (xah-get-fullpath \"../xyz.el\")
in the file at
 /home/mary/emacs/emacs_lib.el
then the return value is
 /home/mary/xyz.el
Regardless how or where emacs_lib.el is called.

This function solves 2 problems.

① If you have file A, that calls the `load' on a file at B, and B calls `load' on file C using a relative path, then Emacs will complain about unable to find C. Because, emacs does not switch current directory with `load'.

To solve this problem, when your code only knows the relative path of another file C, you can use the variable `load-file-name' to get the current file's full path, then use that with the relative path to get a full path of the file you are interested.

② To know the current file's full path, emacs has 2 ways: `load-file-name' and `buffer-file-name'. If the file is loaded by `load', then `load-file-name' works but `buffer-file-name' doesn't. If the file is called by `eval-buffer', then `load-file-name' is nil. You want to be able to get the current file's full path regardless the file is run by `load' or interactively by `eval-buffer'."

  (concat (file-name-directory (or load-file-name buffer-file-name)) @file-relative-path))

(server-start)
(tool-bar-mode 0)

;; We want the active buffer more present then the others
(dimmer-mode)
(setq dimmer-fraction 0.4)
;;https://www.masteringemacs.org/article/disabling-prompts-emacs
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
;; enable window setup history
;; see for alternatives: http://stackoverflow.com/questions/4732511/in-emacs-can-we-maximize-current-active-window-and-then-restore-it-back-in-mul
(when (fboundp 'winner-mode)
      (winner-mode 1))

(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)

(setq column-number-mode t)
;; Please avoid tabs
(setq-default indent-tabs-mode nil)
(set-face-attribute 'default nil :height 140)
;; make auto complete always availabe
(add-hook 'after-init-hook 'global-company-mode)
;; I prefer manual triggered completion
(setq company-idle-delay nil)
;; (set-mark-command) I rarely use and it has a secondary binding C-@
;; so we can use one binding of this command.
(global-set-key (kbd "C-SPC") #'company-complete)
;; we replace switch-to-buffer with helm
(global-set-key (kbd "C-x b") #'helm-mini)
(defalias 'list-buffers 'ibuffer)

;; Configure imenu via helm: lookup buffer contents
;; shortcut derived from Eclipse C-o
(global-set-key (kbd "C-c o") 'helm-imenu)
(setf imenu-auto-rescan t)
;; ================== Helm && Projectile =================
(require 'helm-config)
(helm-mode 1)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
;; automatically regenerate the tags
(setq projectile-idle-timer t)
(setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
(add-hook 'projectile-idle-timer-hook #'projectile-invalidate-cache)

;; exclude maven artifacts
(add-to-list 'projectile-globally-ignored-directories "target")
;; ================== Evil =================
(evil-mode t)
;; add evil movements to magit buffers. This changes some magit key bindings
(require 'evil-magit)
;; https://github.com/Fanael/relative-line-numbers
;(linum-relative-global-mode)
;; http://blog.aaronbieber.com/2016/01/23/living-in-evil.html
(add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
;; help-mode is in evil-motion-state-mode, there for change initial state
;; https://emacs.stackexchange.com/questions/31244/how-can-i-disable-evil-in-help-mode
;; I prefer this because tab for move to next link is hidden.
(evil-set-initial-state 'help-mode 'emacs)
;(add-to-list 'evil-emacs-state-modes 'help-mode)
;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
(fset 'evil-visual-update-x-selection 'ignore)
;; This is not Vim like, but helps to eval last expression for lispy languages
;; Cursor does not move back when switching to normal-state
(setq evil-move-cursor-back nil)
;; enable redo via C-r
(global-undo-tree-mode)
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; ================== Clojure =================
(defun sli-find-tag-default-bounds ()
  "Determine the boundaries of the default tag, based on text at point.
Return a cons cell with the beginning and end of the found tag.
If there is no plausible default, return nil."
  (let (from to bound)
    (when (or (progn
                ;; Look at text around `point'.
                (save-excursion
                  (skip-syntax-backward "w") (setq from (point)))
                (save-excursion
                  (skip-syntax-forward "w") (setq to (point)))
                (> to from))
              ;; Look between `line-beginning-position' and `point'.
              (save-excursion
                (and (setq bound (line-beginning-position))
                     (skip-syntax-backward "^w" bound)
                     (> (setq to (point)) bound)
                     (skip-syntax-backward "w")
                     (setq from (point))))
              ;; Look between `point' and `line-end-position'.
              (save-excursion
                (and (setq bound (line-end-position))
                     (skip-syntax-forward "^w" bound)
                     (< (setq from (point)) bound)
                     (skip-syntax-forward "w")
                     (setq to (point)))))
      (cons from to))))

(require 'clj-refactor)

(defun sli-find-tag-clojure ()
  "Determine default tag to search for, based on text at point.
If there is no plausible default, return nil."
  (let ((bounds (sli-find-tag-default-bounds)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun sli-clojure-mode-init ()
  "For evil mode and clojure the word boundaries are different."
;; https://bitbucket.org/lyro/evil/issues/565/word-commands-do-not-respect-word
  (dolist (c (string-to-list ":_-?!#*"))
    (modify-syntax-entry c "w" clojure-mode-syntax-table)
    (modify-syntax-entry c "w" emacs-lisp-mode-syntax-table))
  ;; (setq find-tag-default-function 'sli-find-tag-clojure)
  (put-clojure-indent 'fact 1)
  (put-clojure-indent 'facts 1)
  ;;https://github.com/clojure-emacs/clj-refactor.el/wiki/installation
  (clj-refactor-mode 1)
  ;; for clojure we replace the evil binding for evil-jump-to-tag
  ;; which resolves a symbol with namespace
  ;;(define-key evil-motion-state-map (kbd "C-]") 'cider-find-var)
  (define-key (current-local-map) (kbd "<f5>") 'cider-find-var)
  (yas/minor-mode 1))

(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'sli-clojure-mode-init)
(add-hook 'clojure-mode-hook #'eldoc-mode)
;; setup config for Cider Repl
(add-hook 'clojure-repl-mode-hook #'paredit-mode)

;; ======== END clojure ==========

;; http://ergoemacs.org/emacs/emacs_highlight_parenthesis.html
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; https://www.emacswiki.org/emacs-test/RecentFiles
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
;; key overrides default binding for find-files-read-only
(global-set-key "\C-x\ \C-r" 'helm-recentf)
(global-set-key "\C-x\ \C-f" 'helm-find-files)
(global-set-key (kbd "C-x g") 'magit-status)

;; Whitepspace
(require 'whitespace)
;(autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
(global-whitespace-mode 1)
(setq whitespace-style (quote
   ( face trailing tabs newline tab-mark ))) ;newline-mark
;; https://www.emacswiki.org/emacs/FillColumnIndicator
(require 'fill-column-indicator)
(setq fci-rule-column 80)

;; fill-paragraph should adhere to this
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'fci-mode)
(setq prog-mode-hook nil)
;; ================= Markdown =================
(add-hook 'markdown-mode-hook 'fci-mode) ; enable fill-column-indicator
;; Normally I write md for github, so use its way of rendering
(setq markdown-command "pandoc -f markdown_github -t html")


;; ================= SQL =================
(setq sql-font-lock-buffers '(sql-mode sql-interactive-mode))
(setq comint-scroll-to-bottom-on-output t)
(if (file-exists-p  "~/.emacs.d/sql.el")
    (load "~/.emacs.d/sql.el"))

(use-package sql
  :config
  (sql-set-product-feature 'postgres
                         :prompt-regexp "^[[:alnum:]_]*=[#>] "))

;;(sql-set-product-feature 'postgres :prompt-length 10)
;; ============= GO ==============
;; http://arenzana.org/2015/Emacs-for-Go/
(defun my-go-mode-hook ()
  (setq compile-command "go build -v && go test -v && go vet && golint")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (go-eldoc-setup)
  (setq gofmt-command "goimports") ;; manages imports
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 4)
  ;(local-set-key (kbd "C-]") 'godef-jump) ;; TODO: evil has C-]
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)
;; autocomplete: TODO: check this again if necessary after adding 'company
;;(ac-config-default)
;;(require 'auto-complete-config)
;; (require 'go-autocomplete)

;; configure Lint
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
;(require 'golint)

;; ============= Lisp ==============
(require 'slime)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(setq inferior-lisp-program "/usr/local/bin/ccl")
(setq slime-contribs '(slime-fancy))

;;; ============ Python ============
(defun sli-python-mode-init ()
  "For evil mode and clojure the word boundaries are differernt."
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)
  (setq find-tag-default-function 'sli-find-tag-clojure))

(add-hook 'python-mode-hook 'sli-python-mode-init)
;; TODO: check if exists ipython
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt -i")

;;; ============ Octave =========

;; this overrides the setting for objc
;; useful for the ML cousera course
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;; ============ ORG ============
(require 'org)
(require 'org-bullets)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
 ;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)")))

(setq org-catch-invisible-edits 'show-and-error)
(setq org-directory "~/Documents/org")
(setq org-agenda-files (list "~/Documents/org/gtd.org"
                             "~/Documents/org/todo.org"
                             "~/Documents/org/privat.org"
                             "~/Documents/org/networking.org"))
(setq sli-notes-files '("~/Documents/org/notes.org"
                        "~/Documents/org/emacs.org"
                        "~/Documents/org/notes/cassandra-training.org"
                        "~/Documents/org/projects.org"
                        "~/Documents/org/notes/customer-inbox.org"))
(setq sli-work-agenda
      (seq-remove (lambda (x) (string-match "privat" x))
                  org-agenda-files))

(setq org-agenda-text-search-extra-files sli-notes-files)
;http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))
                           (sli-notes-files . (:maxlevel . 3))
                           (("~/Documents/org/someday.org") . (:maxlevel . 3))))
;https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-capture-templates
      '(("t" "Todo task inbox" entry
         (file "inbox.org")
         "* TODO %?\n%U")
        ("T" "Todo task (active)" entry
         (file "todo.org")
         "* TODO %^{Title}\n%U\n%?")
        ("r" "read" entry
         (file "inbox.org")
         "* %? :READ:\n%U\n")
        ("n" "note" entry
         (file "inbox.org")
         "* %?\n%U")
        ("j" "Journal" entry
         (file+datetree "no-agenda/worklog.org")
         "*** %?\n%U"
         :tree-type 'week)
        ("h" "Health" entry
         (file+datetree "no-agenda/health.org")
         "*** %?\n%U"
        )))

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
                (tags-todo "-Boss-READ/TODO"
                           ((org-agenda-overriding-header "Next Todos")
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)))
                (tags-todo "+READ/TODO"
                           ((org-agenda-overriding-header "Next Read")
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-files (append org-agenda-files sli-notes-files))))))
              ("w" "My Work List"
               ((agenda ""
                        ((org-agenda-span 'day)
                         (org-agenda-files sli-work-agenda)))
                (tags-todo "-Boss/!-TODO"
                           ((org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t))
                           nil)
                (tags-todo "READ/NEXT"
                           ((org-agenda-files sli-notes-files)
                            (org-agenda-overriding-header "Reading List")))))
              ("p" "Private stuff"
               (;;;(agenda "" ((org-agenda-span 'week)))
                (tags-todo "-READ/TODO"
                           ((org-agenda-files '("~/Documents/org/privat.org")))))))))

(setq org-tag-alist '(("NOTE" . ?n)
                      ("READ" . ?r)
                      ("Qst" . ?f)
                      ("Boss" . ?b)
                      ("Proj" . ?p)
                      ("Geld" . ?g)))
(setq org-tags-exclude-from-inheritance '("NOTE" "Proj"))

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
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

;; Setup Org Babel
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (sh . t)
   (emacs-lisp . t)
   (clojure . t)))

;;(load (xah-get-fullpath "sli-publish.el"))

;;; ============ Java =========

;;  good read: http://www.goldsborough.me/emacs,/java/2016/02/24/22-54-16-setting_up_emacs_for_java_development/
;;; ============ LILYPOND  =========

(setq LilyPond-command-alist '(("LilyPond" "lilypond -o ../target %s" "%s" "%l" "View")
                              ("2PS" "lilypond -f ps %s" "%s" "%p" "ViewPS")
                              ("Book" "lilypond-book %x" "%x" "%l" "LaTeX")
                              ("LaTeX" "latex '\\nonstopmode\\input %l'" "%l" "%d" "ViewDVI")
                              ("View" "xpdf %f")
                              ("ViewPDF" "xpdf %f")
                              ("ViewPS" "gv --watch %p")
                              ("Midi" "timidity %m")
                              ("MidiAll" "")))

;; ===================== other stuff ===========
;; Fix issue in Tramp when executing region
;; Tramp assumes the local TMPDIR exists remotely. WHY? TODO.
;; See John Hitchins remark https://lists.gnu.org/archive/html/emacs-orgmode/2016-01/msg00321.html
;; and https://lists.gnu.org/archive/html/emacs-orgmode/2016-01/msg00282.html
(setq temporary-file-directory "/tmp")
(setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "http://planet.emacsen.org/atom.xml"
        "http://sreweekly.com/feed/"))
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (dichromacy)))
 '(package-selected-packages
   (quote
    (dimmer dockerfile-mode cider org-static-blog company-emacs-eclim eclim octave-mode evil-surround use-package elfeed org-edna htmlize auto-dim-other-buffers company yaml-mode slime org-bullets markdown-mode magit linum-relative helm-projectile go-eldoc go-autocomplete fill-column-indicator clj-refactor))))

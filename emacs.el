(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
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
(package-initialize)

(ensure-package-installed 'ac-cider
                          'cider
                          'clj-refactor
                          'clojure-mode
                          'evil
                          'evil-magit
                          'fill-column-indicator
                          'go-autocomplete
                          'go-eldoc
                          'go-mode
                          'helm
                          'helm-projectile
                          'ibuffer
                          'magit
                          'markdown-mode
                          'paredit
                          'powerline
                          'projectile
                          'rainbow-delimiters
                          'relative-line-numbers
                          'slime
                          'yaml-mode)

(evil-mode t)
;;https://www.masteringemacs.org/article/disabling-prompts-emacs
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(set-face-attribute 'default nil :height 140)
;; https://github.com/Fanael/relative-line-numbers
(global-relative-line-numbers-mode)
;; http://blog.aaronbieber.com/2016/01/23/living-in-evil.html
(add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
(fset 'evil-visual-update-x-selection 'ignore)
;; TODO
;(require 'powerline)
;(powerline-default-theme)
;(powerline-vim-theme)
(defalias 'list-buffers 'ibuffer)
(require 'helm-config)
(helm-mode 1)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(helm-projectile-on) ; this call yielded an ugly output in minibuffer. seems to work.

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

;; ================== Clojure =================
(require 'ac-cider)
(require 'clj-refactor)
;; reload namespace
(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

; TODO: (define-key clojure-mode-map (kbd "F12") 'cider-namespace-refresh)
(global-set-key (kbd "<f12>") 'cider-namespace-refresh)

(defun sli-find-tag-clojure ()
  "Determine default tag to search for, based on text at point.
If there is no plausible default, return nil."
  (let ((bounds (sli-find-tag-default-bounds)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

;; https://bitbucket.org/lyro/evil/issues/565/word-commands-do-not-respect-word
(defun sli-clojure-mode-init ()
  "For evil mode and clojure the word boundaries are different."
  (dolist (c (string-to-list ":_-?!#*"))
    (modify-syntax-entry c "w" clojure-mode-syntax-table)
    (modify-syntax-entry c "w" emacs-lisp-mode-syntax-table))
  (setq find-tag-default-function 'sli-find-tag-clojure)
  (put-clojure-indent 'fact 1)
  (put-clojure-indent 'facts 1)
  ;;https://github.com/clojure-emacs/clj-refactor.el/wiki/installation
  (clj-refactor-mode 1)
  (fci-mode))

(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'sli-clojure-mode-init)
(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

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

;; ================= Markdown =================
(add-hook 'markdown-mode-hook 'fci-mode) ; enable fill-column-indicator

(setq sql-font-lock-buffers '(sql-mode sql-interactive-mode))
(setq comint-scroll-to-bottom-on-output t)


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
;; autocomplete:
(ac-config-default)
(require 'auto-complete-config)
(require 'go-autocomplete)

;; configure Lint
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;; ============= Lisp ==============
(require 'slime)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(setq inferior-lisp-program "/usr/local/bin/ccl")

;;; ============ Python ============
(defun sli-python-mode-init ()
  "For evil mode and clojure the word boundaries are differernt."
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)
  (setq find-tag-default-function 'sli-find-tag-clojure)
  (fci-mode))

(add-hook 'python-mode-hook 'sli-python-mode-init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (dichromacy tsdh-light))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; ============ ORG ============
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

(setq org-catch-invisible-edits 'show-and-error)
(setq org-agenda-files (list "~/Documents/org/"))
(setq org-directory "~/Documents/org")
;http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
;https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-capture-templates
      '(("a" "My TODO task format." entry
         (file "todo.org")
         "* TODO %?\nSCHEDULED: %t\n%a")
        ("n" "note" entry
         (file "todo.org")
         "* %? :NOTE:\n%U\n%a\n")))

;; http://doc.norang.ca/org-mode.html#CustomAgendaViews
(setq org-agenda-custom-commands
      (quote (("n" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t))))))

(setq org-tag-alist '(("NOTE" . ?n)))
(setq org-tags-exclude-from-inheritance '("NOTE"))

;;https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)


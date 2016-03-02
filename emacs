(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

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

(ensure-package-installed 'evil
                          'projectile
                          'magit
			  'ibuffer
			  'clojure-mode
			  'cider
			  'paredit
			  'rainbow-delimiters
			  'relative-line-numbers
			  'powerline
			  'projectile
			  'helm-projectile
			  'helm)

(evil-mode t)
;; https://github.com/Fanael/relative-line-numbers
(global-relative-line-numbers-mode)
;; http://blog.aaronbieber.com/2016/01/23/living-in-evil.html
(add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
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

;; https://bitbucket.org/lyro/evil/issues/565/word-commands-do-not-respect-word
(defun sli-clojure-mode-init ()
  "For evil mode and clojure the word boundaries are differernt."
  (dolist (c (string-to-list ":_-?!#*"))
    (modify-syntax-entry c "w" clojure-mode-syntax-table)
    (modify-syntax-entry c "w" emacs-lisp-mode-syntax-table)))

(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'sli-clojure-mode-init)
;; http://ergoemacs.org/emacs/emacs_highlight_parenthesis.html
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; https://www.emacswiki.org/emacs-test/RecentFiles
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
;; key overrides default binding for find-files-read-only
(global-set-key "\C-x\ \C-r" 'helm-recentf)

(global-set-key (kbd "C-x g") 'magit-status)

;; Whitepspace
(require 'whitespace)
;(autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
(global-whitespace-mode 1)
(setq whitespace-style (quote
   ( face trailing tabs newline tab-mark ))) ;newline-mark

;; reload namespace
(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

; TODO: (define-key clojure-mode-map (kbd "F12") 'cider-namespace-refresh)
(global-set-key (kbd "<f12>") 'cider-namespace-refresh)

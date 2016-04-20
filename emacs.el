(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; https://www.masteringemacs.org/article/what-is-new-in-emacs-24-part-2
;(setq package-enable-at-startup nil)
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
                          'ac-cider
                          'paredit
                          'markdown-mode
                          'rainbow-delimiters
                          'relative-line-numbers
                          'powerline
                          'projectile
                          'helm-projectile
                          'helm)

(evil-mode t)
(windmove-default-keybindings)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(set-face-attribute 'default nil :height 140)
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

(defun sli-find-tag-clojure ()
  "Determine default tag to search for, based on text at point.
If there is no plausible default, return nil."
  (let ((bounds (sli-find-tag-default-bounds)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

;; https://bitbucket.org/lyro/evil/issues/565/word-commands-do-not-respect-word
(defun sli-clojure-mode-init ()
  "For evil mode and clojure the word boundaries are differernt."
  (dolist (c (string-to-list ":_-?!#*"))
    (modify-syntax-entry c "w" clojure-mode-syntax-table)
    (modify-syntax-entry c "w" emacs-lisp-mode-syntax-table))
  (setq find-tag-default-function 'sli-find-tag-clojure))

;(require 'ac-cider)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'sli-clojure-mode-init)
(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

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


(setq sql-font-lock-buffers '(sql-mode sql-interactive-mode))
(setq comint-scroll-to-bottom-on-output t)

;(require 'markdown-mode)

;; ============= Lisp ==============
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)


;;; ============ Python ============
(defun sli-python-mode-init ()
  "For evil mode and clojure the word boundaries are differernt."
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)
  (setq find-tag-default-function 'sli-find-tag-clojure))

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
(setq org-catch-invisible-edits 'show-and-error)
(setq org-agenda-files (list "/Users/slitsche/Documents/org/"))

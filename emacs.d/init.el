(require 'package)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-refresh-contents)

;; https://www.masteringemacs.org/article/what-is-new-in-emacs-24-part-2
(setq package-enable-at-startup nil)
(package-install-selected-packages t)

;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))

;; ================== Evil =================

;; help-mode is in evil-motion-state-mode, there for change initial state
;; https://emacs.stackexchange.com/questions/31244/how-can-i-disable-evil-in-help-mode
;; I prefer this because tab for move to next link is hidden.
;; (evil-set-initial-state 'help-mode 'emacs)

;; This is not Vim like, but helps to eval last expression for lispy languages
;; Cursor does not move back when switching to normal-state
;; (setq evil-move-cursor-back nil)

;; enable redo via C-r
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package evil
:init
(progn
    (setq evil-undo-system 'undo-tree)
    ;; `evil-collection' assumes `evil-want-keybinding' is set to
    ;; `nil' before loading `evil' and `evil-collection'
    ;; @see https://github.com/emacs-evil/evil-collection#installation
    (setq evil-want-keybinding nil)
    )
:config
(progn
  (evil-mode 1)
  ;; http://blog.aaronbieber.com/2016/01/23/living-in-evil.html
  (add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'info-mode)
  ;; http://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
  (fset 'evil-visual-update-x-selection 'ignore)
  ))

(use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; ================== General stuff =================
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
(menu-bar-mode 0)
(auto-save-visited-mode 1)

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

;; ================= General Editing  =====================

(setq column-number-mode t)
(global-hl-line-mode 1)
;; Please avoid tabs
(setq-default indent-tabs-mode nil)
(set-face-attribute 'default nil :height 140)
;; make auto complete always available
(add-hook 'after-init-hook 'global-company-mode)
;; I prefer manual triggered completion
(setq company-idle-delay nil)
;; This will require a key binding.
;; C-SPC would be nice.  By default it is bound to
;; (set-mark-command) I rarely use and it has a secondary binding C-@
;; so we can use one binding of this command.
(global-set-key (kbd "C-SPC") #'company-complete)
(defalias 'list-buffers 'ibuffer)

(setf imenu-auto-rescan t)

;; http://ergoemacs.org/emacs/emacs_highlight_parenthesis.html
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; https://www.emacswiki.org/emacs/RecentFiles
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(global-set-key (kbd "C-x g") 'magit-status)

;; Whitepspace
(require 'whitespace)
;Toggle whitespace visualization.
(global-whitespace-mode 1)
(setq whitespace-style (quote
   ( face trailing tabs newline tab-mark ))) ;newline-mark

;; Mostly I want to avoid trailing whitespace.  That's why I want to clean up
;; those always.  I could only do harm when working on other people files.
;; Then we could git revert and introduce a whitespace commit.
;; https://emacs.stackexchange.com/questions/14466/how-to-run-an-after-save-hook-only-when-the-buffer-has-been-saved-manually
;; 2022-07-17
;; Now I prefer auto-save-visited-mode.  Its call to save is not a command.
;; therefore the value of `this-command` is nil.  Disabling it and observing negative effects.
;; It means the function could be simplified to check whether `this-command` is non nil.
;; (defun sli-before-save-action ()
;;   "Used in `before-save-hook`.  Triggered only for save actions from `save-buffer`."
;;   (when (memq this-command '(save-buffer save-some-buffers))
;;     (delete-trailing-whitespace)))
;; 2025-06-27 (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; https://www.emacswiki.org/emacs/FillColumnIndicator
(use-package fill-column-indicator
  :ensure t
  :init
  (setq fci-rule-column 80))

;; fill-paragraph should adhere to this
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'fci-mode)

;; ================== Helm && Projectile =================
(use-package helm
  :ensure t
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini) ;; replace switch-to-buffer
         ("C-x C-f" . helm-find-files)
         ("C-x C-d" . helm-browse-project)  ;; TODO: does this conflict with projectile?
         ;; Configure imenu via helm: lookup buffer contents
         ;; The key binding is derived from Eclipse C-o
         ("C-c o"   . helm-imenu)
         ;; key overrides default binding for find-files-read-only
         ("C-x C-r" . helm-recentf)
         ("M-y" . helm-show-kill-ring))
  :config  (helm-mode 1)
           (setq helm-M-x-fuzzy-match 1)
           (setq helm-buffers-fuzzy-matching 1)
           (setq helm-recentf-fuzzy-match 1)
  )
;; (require 'helm)
;; (helm-mode 1)

(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (setq projectile-enable-caching t)
  ;; automatically regenerate the tags
  ;; (setq projectile-enable-idle-timer t)
  (setq projectile-create-missing-test-files t)
  ;; Addressing issue for sbt: https://github.com/bbatsov/projectile/issues/1650
  (projectile-update-project-type
     'sbt
     :src-dir
     (lambda (file-path) (projectile-complementary-dir file-path "test" "main"))
     :test-dir
     (lambda (file-path) (projectile-complementary-dir file-path "main" "test")))
  (setq projectile-mode-line-prefix "P")
  ;(setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
  (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))
  (projectile-mode +1))

(use-package helm-projectile
  :config
  (helm-projectile-on))

;; ================== Clojure =================
;; refactor is out of sync.  disable it for now
;;(require 'clj-refactor)
;;(setq cljr-inject-dependencies-at-jack-in t)
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
  ;; (clj-refactor-mode 1)
  ;; for clojure we replace the evil binding for evil-jump-to-tag
  ;; which resolves a symbol with namespace
  ;;(define-key evil-motion-state-map (kbd "C-]") 'cider-find-var)
  (define-key (current-local-map) (kbd "<f5>") 'cider-find-var)
  ;;(yas/minor-mode 1)
  )

(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'sli-clojure-mode-init)
(add-hook 'clojure-mode-hook #'eldoc-mode)
;; setup config for Cider Repl
(add-hook 'clojure-repl-mode-hook #'paredit-mode)

;; ================= Scala =================
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")
;; After upgrade to sbt 1 the sbt prompt showed escapt characters.
;; the simplest way to get rid of it is using sbt-mode
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
;; ================= Markdown =================
(add-hook 'markdown-mode-hook 'fci-mode) ; enable fill-column-indicator
;; Normally I write md for github, so use its way of rendering
(setq markdown-command "pandoc -f markdown_github -t html")

(use-package highlight-indentation
  :init (add-hook 'yaml-mode-hook #'highlight-indentation-mode))

;; ================= SQL =================
(setq sql-font-lock-buffers '(sql-mode sql-interactive-mode))
(setq comint-scroll-to-bottom-on-output t)
(if (file-exists-p  "~/.emacs.d/sql.el")
    (load "~/.emacs.d/sql.el"))

(use-package sql
  :config
  (sql-set-product-feature 'postgres
                         :prompt-regexp "^[[:alnum:]_]*=[#>] "))


;; ============= Lisp ==============
;; (require 'slime)
;; (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
;; (add-hook 'lisp-mode-hook 'paredit-mode)
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
;; (setq slime-contribs '(slime-fancy))

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
;; useful for the ML cousera course

;; this overrides the setting for objc
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;; ============ ORG ============

(load   "~/.emacs.d/orgconf.el")

;;; ============ Java =========

(defun sli-java-mode-config ()
  (setq-local tab-width 2))

(add-hook 'java-mode-hook 'sli-java-mode-config)

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
(setq calendar-location-name "Berlin, Germany")
(setq calendar-latitude 52.52)
(setq calendar-longitude 13.40)
;; (require 'theme-changer)
;; (change-theme 'dichromacy 'tsdh-dark)
;;

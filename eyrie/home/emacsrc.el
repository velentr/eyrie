;;; init.el --- emacs configuration

;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

;;; Commentary:

;;; Code:

(package-initialize)

;; quality of life improvements
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(menu-bar-mode -1)
(global-display-line-numbers-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'find-file-hook
          #'(lambda ()
              (let ((proj (project-current)))
                (if proj
                    (setq default-directory (project-root proj)))
                (if (string-match "~/aircam[0-9]*" default-directory)
                    (progn
                      (setq exec-path
                            (append
                             exec-path
                             (list
                              (concat
                               default-directory
                               "/build/python3_venv/bin"))))
                      (setq format-all-formatters
                            (cons '("Python" ruff)
                                  format-all-formatters)))))))

(setq-default indent-tabs-mode nil)

(setq auto-mode-alist
      (append
       '(("\\.g-hooks" . scheme-mode)           ; g-hooks are always written in scheme
         ("BUILD\\.bazel" . bazel-build-mode)   ; aircam has some build files with a prefix
         ("\\.ml" . tuareg-mode))               ; .ml is ocaml not lisp...
       ;; git-rebase-mode is stupidly more complex than just editing in
       ;; fundamental mode with evil
       (rassq-delete-all 'git-rebase-mode auto-mode-alist)))

(defun sane-c-mode ()
  "Set up sane settings for editing c code."
  (c-set-style "linux")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (set-fill-column 80))
(add-hook 'c-mode-hook 'sane-c-mode)

(defun sane-dts-mode ()
  "Set up sane settings for editing dts code."
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (set-fill-column 80))
(add-hook 'dts-mode-hook 'sane-dts-mode)

(defun sane-sh-mode ()
  "Set up sane settings for editing sh code."
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (set-fill-column 80))
(add-hook 'sh-mode-hook 'sane-sh-mode)

(defun sane-elisp-mode ()
  "Sane defaults for elisp."
  (setq indent-tabs-mode nil)
  (set-fill-column 80))
(add-hook 'emacs-lisp-mode-hook 'sane-elisp-mode)

(defun sane-scheme-mode ()
  "Sane defaults for scheme."
  (setq indent-tabs-mode nil)
  (set-fill-column 80)
  (let ((indentations
         ;; taken from guix's .dir-locals.el
         '((eval-when . 1)
           (call-with-prompt . 1)
           (test-assert . 1)
           (test-assertm . 2)
           (test-equalm . 1)
           (test-equal . 1)
           (test-eq . 1)
           (call-with-input-string . 1)
           (call-with-port . 1)
           (guard . 1)
           (lambda* . 1)
           (substitute* . 1)
           (match-record . 3)
           (match-record-lambda . 2)
           (let-keywords . 3)
           (modify-inputs . 1)
           (replace . 1)
           (modify-phases . 1)
           (replace . 1)
           (add-before . 2)
           (add-after . 2)
           (modify-services . 1)
           (with-directory-excursion . 1)
           (with-file-lock . 1)
           (with-file-lock/no-wait . 1)
           (with-profile-lock . 1)
           (with-writable-file . 2)
           (package . 0)
           (package/inherit . 1)
           (origin . 0)
           (build-system . 0)
           (bag . 0)
           (graft . 0)
           (operating-system . 0)
           (file-system . 0)
           (manifest-entry . 0)
           (manifest-pattern . 0)
           (substitute-keyword-arguments . 1)
           (with-store . 1)
           (with-external-store . 1)
           (with-error-handling . 0)
           (with-mutex . 1)
           (with-atomic-file-output . 1)
           (call-with-compressed-output-port . 2)
           (call-with-decompressed-port . 2)
           (call-with-gzip-input-port . 1)
           (call-with-gzip-output-port . 1)
           (call-with-lzip-input-port . 1)
           (call-with-lzip-output-port . 1)
           (signature-case . 1)
           (emacs-batch-eval . 0)
           (emacs-batch-edit-file . 1)
           (emacs-substitute-sexps . 1)
           (emacs-substitute-variables . 1)
           (with-derivation-narinfo . 1)
           (with-derivation-substitute . 2)
           (with-status-report . 1)
           (with-status-verbosity . 1)
           (with-build-handler . 1)
           (mlambda . 1)
           (mlambdaq . 1)
           (syntax-parameterize . 1)
           (with-monad . 1)
           (mbegin . 1)
           (mwhen . 1)
           (munless . 1)
           (mlet* . 2)
           (mlet . 2)
           (mparameterize . 2)
           (run-with-store . 1)
           (run-with-state . 1)
           (wrap-program . 1)
           (wrap-script . 1)
           (with-imported-modules . 1)
           (with-extensions . 1)
           (with-parameters . 1)
           (let-system . 1)
           (with-build-variables . 2)
           (with-database . 2)
           (call-with-database . 1)
           (call-with-transaction . 1)
           (with-statement . 3)
           (call-with-retrying-transaction . 1)
           (call-with-savepoint . 1)
           (call-with-retrying-savepoint . 1)
           (call-with-container . 1)
           (container-excursion . 1)
           (eventually . 1)
           (call-with-progress-reporter . 1)
           (with-repository . 2)
           (with-temporary-git-repository . 2)
           (with-environment-variables . 1)
           (with-fresh-gnupg-setup . 1)
           (with-paginated-output-port . 1)
           (with-shepherd-action . 3)
           (with-http-server . 1))))
    (dolist (indentation indentations)
      (put (car indentation) 'scheme-indent-function (cdr indentation)))))
(add-hook 'scheme-mode-hook 'sane-scheme-mode)

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(defun sane-lua-mode ()
  "Sane defaults for lua."
  (setq indent-tabs-mode nil)
  (set-fill-column 80))
(add-hook 'lua-mode-hook 'sane-lua-mode)

(defun sane-bazel-mode ()
  "Sane defaults for bazel."
  (setq indent-tabs-mode nil)
  (set-fill-column 100))
(add-hook 'bazel-mode-hook 'sane-bazel-mode)

;; lsp setup
(require 'eglot)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook
          (lambda ()
            (combobulate-mode t)
            (keymap-set python-ts-mode-map "C-j" 'combobulate-drag-down)
            (keymap-set python-ts-mode-map "C-k" 'combobulate-drag-up)
            (keymap-set evil-normal-state-map "H" 'combobulate-navigate-previous)
            (keymap-set evil-normal-state-map "L" 'combobulate-navigate-next)
            (keymap-set evil-normal-state-map "J" 'combobulate-navigate-down-list-maybe)
            (keymap-set evil-normal-state-map "K" 'combobulate-navigate-up-list-maybe)
            (eglot-ensure)))
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'tuareg-mode-hook 'eglot-ensure)

;; treesit setup
(require 'treesit)

(setq major-mode-remap-alist
      '((c++-mode . c++-ts-mode)
        (python-mode . python-ts-mode)
        (rust-mode . rust-ts-mode)))

(define-key evil-outer-text-objects-map "f"
            (evil-textobj-tree-sitter-get-textobj "function.outer"))
(define-key evil-inner-text-objects-map "f"
            (evil-textobj-tree-sitter-get-textobj "function.inner"))
(define-key evil-outer-text-objects-map "o"
            (evil-textobj-tree-sitter-get-textobj "class.outer"))
(define-key evil-inner-text-objects-map "o"
            (evil-textobj-tree-sitter-get-textobj "class.inner"))

;; emacs only defines 8 colors by default; define the other 8 using solarized
;; colors
(tty-color-define "brightblack"    8 '(  0  43  54))
(tty-color-define "brightred"      9 '(203  75  22))
(tty-color-define "brightgreen"   10 '( 88 110 117))
(tty-color-define "brightyellow"  11 '(101 123 131))
(tty-color-define "brightblue"    12 '(131 148 150))
(tty-color-define "brightmagenta" 13 '(108 113 196))
(tty-color-define "brightcyan"    14 '(147 161 161))
(tty-color-define "brightwhite"   15 '(253 246 227))

(load-theme 'solarized t)
(set-terminal-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; make code review a little less terrible
(require 'github)

(require 'evil)
(evil-mode 1)

(require 'company)
(add-hook 'after-init-hook #'global-company-mode)

(require 'format-all)
(add-hook 'prog-mode-hook #'format-all-mode)
(add-hook 'format-all-mode-hook #'format-all-ensure-formatter)


;; leader commands
(defvar leader-map (make-sparse-keymap)
  "Keymap for leader sequences.")
(define-key evil-motion-state-map " " leader-map)

;; find tags
(define-key leader-map "t" #'xref-find-definitions)
(define-key leader-map "y" #'xref-go-back)

;; scan through flycheck errors
(define-key leader-map "[" (kbd "C-c ! p"))
(define-key leader-map "]" (kbd "C-c ! n"))

;; search for a file in the git repo
(define-key leader-map "s" #'project-find-file)

;; rebinding some of the github stuff to work better with evil-mode
(define-key leader-map "g" #'gh-open-buffer)
(define-key leader-map "r" #'gh-refresh-buffer)
(define-key leader-map "m" #'gh-mergequeue)

;; navigating windows
(define-key leader-map "o" #'other-window)
(define-key leader-map "e" #'switch-to-buffer)

;; opening the todo list
(define-key leader-map "a" #'org-agenda)

(define-key leader-map (kbd "u")
  (lambda ()
    (interactive)
    (cond
     ((eq major-mode 'github-mode)
      (gh-move-up-buffer))
     ((member major-mode '(c-mode c-ts-mode c++-mode c++-ts-mode))
      (cond
       ((string-suffix-p ".h" buffer-file-name)
        (find-file (concat (substring buffer-file-name nil -2) ".cc")))
       ((string-suffix-p ".c" buffer-file-name)
        (find-file (concat (substring buffer-file-name nil -2) ".h")))
       ((string-suffix-p ".cc" buffer-file-name)
        (find-file (concat (substring buffer-file-name nil -3) ".h"))))))))

(define-key leader-map (kbd "RET")
  (lambda ()
    (interactive)
    (cond
     ((eq major-mode 'github-mode)
      (gh-select-pr)))))


;; editing sources with unicode
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; need to easily insert some unicode characters for eg Julia code
(defconst unicode-char-map
  '(("alpha"   . "α")
    ("beta"    . "β")
    ("Gamma"   . "Γ")
    ("gamma"   . "γ")
    ("Delta"   . "Δ")
    ("delta"   . "δ")
    ("epsilon" . "ε")
    ("Pi"      . "Π")
    ("pi"      . "π")
    ("rho"     . "ρ")
    ("Sigma"   . "Σ")
    ("sigma"   . "σ")
    ("Omega"   . "Ω")
    ("omega"   . "ω"))
  "Mapping for easier insertion of unicode characters.")
(defun unicode-name-to-char (name)
  "Convert a unicode NAME into the correct unicode character."
  (interactive (list (thing-at-point 'word 'no-properties)))
  (backward-kill-word 1)
  (insert (alist-get name unicode-char-map name nil 'equal)))
(define-key evil-insert-state-map (kbd "C-g") #'unicode-name-to-char)

;; load TAGS from cwd
(add-to-list 'tags-table-list "./TAGS")

(custom-set-variables
 ;; wtf does emacs think my terminal is light?
 '(frame-background-mode 'dark)
 '(sh-basic-offset 8)
 '(lua-indent-level 4)
 '(org-startup-folded t)
 ;; C-i bound to evil-jump-forward breaks org-tab in the terminal
 '(evil-want-C-i-jump nil))

(setq org-agenda-files '("~/todo"))
(setq org-agenda-start-with-follow-mode t)
(setq org-hierarchical-todo-statistics nil)

;; better keybindings in org-mode (especially the org-agenda keybindings)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(require 'org-lifelist)

;;; init.el ends here

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
(global-linum-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)

;; git-rebase-mode is stupidly more complex than just editing in fundamental
;; mode with evil
(setq auto-mode-alist (rassq-delete-all 'git-rebase-mode auto-mode-alist))

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
  (set-fill-column 80))
(add-hook 'scheme-mode-hook 'sane-scheme-mode)

(defun sane-lua-mode ()
  "Sane defaults for lua."
  (setq indent-tabs-mode nil)
  (set-fill-column 80))
(add-hook 'lua-mode-hook 'sane-lua-mode)

(defun sane-erlang-mode ()
  "Sane defaults for erlang."
  (setq indent-tabs-mode nil)
  (set-fill-column 80))
(add-hook 'erlang-mode-hook 'sane-erlang-mode)

(defun sane-bazel-mode ()
  "Sane defaults for bazel."
  (setq indent-tabs-mode nil)
  (set-fill-column 100))
(add-hook 'bazel-mode-hook 'sane-bazel-mode)

;; lsp-mode setup
(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)

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

(require 'worklog)

(require 'evil)
(evil-mode 1)

(require 'company)
(add-hook 'after-init-hook #'global-company-mode)


;; Search through the currect git repo for the file matching a glob.
(defun repo-search-ret ()
  "Quit the *repo-search* buffer, opening the file under the cursor."
  (interactive)
  (let ((file (thing-at-point 'filename)))
    (quit-window)
    (find-file file)))
(defvar repo-search-map
  (let ((map (make-sparse-keymap)))
    ;; Note that this only works in evil mode; otherwise, (kbd "RET") should be
    ;; used.
    (define-key map [remap evil-ret] 'repo-search-ret)
    map)
  "Keymap for repo-search minor mode.")

(define-minor-mode repo-search-mode
  "Minor mode for file selection during repo-search."
  :init-value nil
  :lighter " RepoSearch"
  :keymap repo-search-map)

(defun repo-search (glob)
  "Search through the git repo in the cwd for a file matching GLOB."
  (interactive "sRepo search glob: ")
  (with-output-to-temp-buffer
      "*repo-search*"
    (call-process
     "git" nil standard-output nil "--no-pager" "ls-files" "--" (concat "*" glob "*"))
    (switch-to-buffer-other-window "*repo-search*")
    (repo-search-mode t)))


;; leader commands
(defvar leader-map (make-sparse-keymap)
  "Keymap for leader sequences.")
(define-key evil-motion-state-map " " leader-map)

;; find tags
(define-key leader-map "t" #'xref-find-definitions)
(define-key leader-map "r" #'xref-pop-marker-stack)

;; scan through flycheck errors
(define-key leader-map "[" (kbd "C-c ! p"))
(define-key leader-map "]" (kbd "C-c ! n"))

;; run code_format
(define-key leader-map "f" #'aircam-code-format)

;; search for a file in the git repo
(define-key leader-map "s" #'repo-search)

;; rebinding some of the github stuff to work better with evil-mode
(define-key leader-map "g" #'gh-open-buffer)
(define-key leader-map "r" #'gh-refresh-buffer)
(define-key leader-map "m" #'gh-mergequeue)

;; bindings for worklogs
(define-key leader-map "d" #'worklog-dashboard)

;; navigating windows
(define-key leader-map "o" #'other-window)
(define-key leader-map "e" #'switch-to-buffer)

(define-key leader-map (kbd "u")
  (lambda ()
    (interactive)
    (cond
     ((eq major-mode 'github-mode)
      (gh-move-up-buffer))
     ((or (eq major-mode 'c-mode)
          (eq major-mode 'c++-mode))
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
      (gh-select-pr))
     ((eq major-mode 'worklog-mode)
      (worklog-open)))))


;; set up org-roam for zettelkasten
(require 'org-roam)
(setq org-roam-directory (file-truename "~/src/slip-box"))
(org-roam-db-autosync-mode)


;; set up editing aircam sources
(require 'aircam)
(dir-locals-set-class-variables
 'aircam
 `((c-mode      . ((mode . c++) ; for header files
                   (eval . (aircam-c++-mode))))
   (c++-mode    . ((eval . (aircam-c++-mode))))
   (python-mode . ((eval . (aircam-py-mode))))))
(dir-locals-set-directory-class "~/aircam" 'aircam)

(setq jiralib-url "https://skydio.atlassian.net")

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
 '(org-startup-folded t))

;;; init.el ends here

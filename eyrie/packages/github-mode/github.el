;;; github --- interact with github pull requests

;;; SPDX-FileCopyrightText: 2021 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-2.0-only

;;; Commentary:

;; Github code reviews in Emacs

;;; Code:

(require 'ghub)
(require 'seq)

(defvar gh-owner
  "Skydio"
  "Owner of the github repo to query for reviews.")

(defvar gh-repo
  "aircam"
  "Github repo to query for reviews.")

(defvar gh-user
  "brian-kubisiak-skydio"
  "Github account username.")

(defvar gh-summary-buffer-name
  "*github-summary*"
  "Buffer name to use for the github summary page.")

(defun gh-pr-buffer-name (number)
  "Get the buffer name for the pr NUMBER."
  (format "*github-pr-%d*" number))

(defun gh-is-pr-buffer (name)
  "If NAME is a pr buffer, return the pr number; else, return nil."
  (cond
   ((string-match
     (rx "*github-pr-" (group (one-or-more digit)) "*")
     name)
    (string-to-number (match-string 1 name)))
   (t nil)))

(defun gh-summary-query ()
  "Retrieve the graphql query to use for getting the pr overview."
  ;; TODO: get test results too
  (let ((querystring
         (format
          "query:\"is:pr is:open involves:@me repo:%s/%s sort:updated-desc\""
          gh-owner gh-repo)))
    (gql-query `("query"
                 (("search" "last:100" ,querystring "type:ISSUE")
                  ("edges"
                   ("node"
                    ("... on PullRequest"
                     ("author" "login")
                     "title"
                     (("labels" "last:8")
                      ("edges"
                       ("node"
                        "name")))
                     "number"
                     (("reviews" "last:100")
                      ("edges"
                       ("node"
                        "state")))))))))))

(defun gh--parse-summary-query (query)
  "Parse the QUERY data from a graphql pr request."
  (let ((data (alist-get 'edges (alist-get 'search (alist-get 'data query)))))
    (seq-map 'gh--parse-summary-queryline data)))

(defun gh--parse-summary-queryline (queryline)
  "Convert QUERYLINE into a readable line for the output buffer."
  (let* ((data (alist-get 'node queryline))
         (number (alist-get 'number data))
         (title (alist-get 'title data))
         (author (alist-get 'login (alist-get 'author data)))
         (reviews (alist-get 'edges (alist-get 'reviews data)))
         (labels
          (seq-map
           (lambda (label)
             (alist-get 'name (alist-get 'node label)))
           (alist-get 'edges (alist-get 'labels data)))))
    (list
     number
     title
     author
     (gh--count-approvals reviews)
     (gh--in-merge-queue labels)
     (gh--pr-size labels)
     (gh--filter-labels labels))))

(defun gh-pr-query (number)
  "Retrieve the graphql query for getting an overview of a specific pr NUMBER."
  (let ((ownerstring (format "owner:\"%s\"" gh-owner))
        (repostring (format "name:\"%s\"" gh-repo))
        (prnumber (format "number:%d" number)))
    (gql-query `("query"
                 (("repository" ,ownerstring ,repostring)
                  (("pullRequest" ,prnumber)
                   "title"
                   "body"
                   "baseRefName"
                   "headRefOid"
                   ("author" "login")
                   (("files" "first:100")
                    ("edges"
                     ("node"
                      "path"
                      "additions"
                      "deletions")))))))))

(defun gh--parse-pr-query (query)
  "Parse the QUERY result for a pr."
  (let* ((data (alist-get 'pullRequest
                          (alist-get 'repository
                                     (alist-get 'data query))))
         (title (alist-get 'title data))
         (body (replace-regexp-in-string "\r" "" (alist-get 'body data)))
         (branch-name (alist-get 'baseRefName data))
         (commit (alist-get 'headRefOid data))
         (author (alist-get 'login (alist-get 'author data)))
         (files (alist-get 'edges (alist-get 'files data))))
    (list
     commit
     branch-name
     author
     title
     body
     (seq-map (lambda (file)
                (let ((data (alist-get 'node file)))
                  (list (alist-get 'path data)
                        (alist-get 'additions data)
                        (alist-get 'deletions data))))
              files))))

(defun gh--filter-labels (labels)
  "Remove merge-queue and size from LABELS."
  (seq-filter
   (lambda (label)
     (not (or (equal "1 merge-queue" label)
              (string-prefix-p "size/" label))))
   labels))

(defun gh--in-merge-queue (labels)
  "Check if LABELS contain the merge-queue label."
  (seq-some
   (lambda (label)
     (equal label "1 merge-queue"))
   labels))

(defun gh--pr-size (labels)
  "Get the pr size from the given LABELS."
  (seq-some
   (lambda (label)
     (if (string-prefix-p "size/" label)
         (substring label 5)
       nil))
   labels))

(defun gh--count-approvals (reviews)
  "Count how many REVIEWS are approvals."
  (seq-reduce
   (lambda (acc review)
     "Accumulate approved reviews."
     (let* ((data (alist-get 'node review))
            (state (alist-get 'state data)))
       (if (equal state "APPROVED")
           (+ acc 1)
         acc)))
   reviews
   0))

(defun gh--load-summary ()
  "Query the graphql api for pr data."
  (let ((json-data (ghub-graphql (gh-summary-query))))
    (gh--parse-summary-query json-data)))

(defun gh--load-pr (number)
  "Query the graphql api for an overview of a specific pr NUMBER."
  (let ((json-data (ghub-graphql (gh-pr-query number))))
    (gh--parse-pr-query json-data)))

(defun gh--format-pr-title (title author)
  "Format TITLE and AUTHOR to a suitable fixed-width format for the pr buffer."
  (let ((width 70)
        (titleauthor (format "%s  (%s)" title author)))
    (if (< (length titleauthor) width)
        (format "%-70s" titleauthor)
      (concat (substring titleauthor 0 (- width 3)) "..."))))

(defun gh--format-labels (labels)
  "Format LABELS for inserting into the pr buffer."
  (mapconcat 'identity labels ","))

(defun gh--insert-pr-data (pr)
  "Insert PR data into the current buffer."
  (let* ((number (car pr))
         (nrest (cdr pr))
         (title (car nrest))
         (trest (cdr nrest))
         (author (car trest))
         (arest (cdr trest))
         (approvals (car arest))
         (aprest (cdr arest))
         (in-mergequeue (car aprest))
         (mrest (cdr aprest))
         (size (car mrest))
         (srest (cdr mrest))
         (labels (car srest)))
  (insert (format "%6d" number) " "
          (gh--format-pr-title title author) " "
          (format "[%2d]" approvals) " "
          (format "[%s]" (if in-mergequeue "X" " ")) " "
          (format "%3s" (if size size "*")) " "
          (gh--format-labels labels) "\n")))

(defun gh--insert-pr-summary (pr)
  "Insert PR summary data into the current buffer."
  (let* ((commit (car pr))
         (crest (cdr pr))
         (branch (car crest))
         (brest (cdr crest))
         (author (car brest))
         (arest (cdr brest))
         (title (car arest))
         (trest (cdr arest))
         (body (car trest))
         (borest (cdr trest))
         (files (car borest)))
    (insert
     (format "branch: %s\n" branch)
     (format "commit: %s\n" commit)
     (format "author: %s\n\n" author)
     (format "%s\n\n%s\n\n" title body))
    (seq-do (lambda (file)
              (insert (gh--summarize-file file)))
            files)))

(defun gh--summarize-file (file)
  "Generate a string summarizing FILE, similar to git diff --stat."
  (let ((path (car file))
        (additions (cadr file))
        (deletions (caddr file)))
    (format "%-80s | +%5d -%5d\n" path additions deletions)))

(defun gh-mergequeue ()
  "Add a change to the merge queue."
  (interactive)
  (let* ((name (buffer-name))
         (pr-number
          (cond
           ((equal name gh-summary-buffer-name)
            (string-to-number (thing-at-point 'line)))
           ((gh-is-pr-buffer name)
            (gh-is-pr-buffer name)))))
    (if (not (equal nil pr-number))
        (call-process "gh" nil nil nil
                      "--repo" (format "%s/%s" gh-owner gh-repo)
                      "pr"
                      "edit"
                      (format "%d" pr-number)
                      "--add-label" "1 merge-queue"))))

(defun gh-refresh-buffer ()
  "Refresh the github data in the current buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (name (buffer-name))
        (p (point)))
    (cond
     ((equal name gh-summary-buffer-name)
      (gh--refresh-summary))
     ((gh-is-pr-buffer name)
      (gh--refresh-pr (gh-is-pr-buffer name)))
     (t (message "unrecognized github buffer")))
    (goto-char p)))

(defun gh-move-up-buffer ()
  "Move 'up' one level in the page heirarchy."
  (interactive)
  (let ((name (buffer-name)))
    (cond
     ((gh-is-pr-buffer name)
      (gh-open-buffer))
     (t (message (format "can't move up from %s" name))))))

(defun gh--refresh-summary ()
  "Refresh the current buffer with toplevel pr data."
  (erase-buffer)
  (let ((pr-data (gh--load-summary)))
    (insert "* outgoing\n\n")
    (seq-do 'gh--insert-pr-data
            (seq-filter (lambda (pr)
                          (equal gh-user (caddr pr)))
                        pr-data))
    (insert "\n\n* incoming\n\n")
    (seq-do 'gh--insert-pr-data
            (seq-filter (lambda (pr)
                          (not (equal gh-user (caddr pr))))
                        pr-data))))

(defun gh--refresh-pr (number)
  "Refresh the current buffer with a summary of pr NUMBER."
  (erase-buffer)
  (let ((pr-data (gh--load-pr number)))
    (gh--insert-pr-summary pr-data)))

(defun gh-open-buffer ()
  "Open a new github-mode buffer."
  (interactive)
  (let ((buffer (get-buffer-create gh-summary-buffer-name)))
    (switch-to-buffer buffer)
    (if (= (buffer-size buffer) 0)
        (gh--refresh-summary))
    (github-mode)))

(defun gh-open-pr (pr-number)
  "Open a summary of PR-NUMBER in a new buffer."
  (interactive "npr number: ")
  (let ((buffer (get-buffer-create (gh-pr-buffer-name pr-number))))
    (switch-to-buffer buffer)
    (if (= (buffer-size buffer) 0)
        (gh--refresh-pr pr-number))
    (github-mode)))

(defun gh-select-pr ()
  "Open a pr summary for the pr specified in the line at point."
  (interactive)
  (let* ((pr-oneline (thing-at-point 'line))
         (pr-number (string-to-number pr-oneline)))
    (if (eq pr-number 0)  ;; user selected an invalid line
        nil
      (gh-open-pr pr-number))))


(defvar github-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x RET") 'gh-select-pr)
    (define-key map (kbd "C-x u") 'gh-move-up-buffer)
    map)
  "Keymap for github-mode.")

(defface github-mode-ok-face
  '((t :foreground "green"))
  "Green face for highlighting mq+1, cr+2, etc."
  :group 'github-mode)

(defface github-mode-warn-face
  '((t :foreground "yellow"))
  "Yellow face for highlighting warnings like no crs, large changes, etc."
  :group 'github-mode)

(defface github-mode-err-face
  '((t :foreground "red"))
  "Red face for severe warnings like XXL changes, etc."
  :group 'github-mode)

(defvar github-mode-summary-font-lock-defaults
  '((("^* outgoing$" . font-lock-function-name-face)
     ("^* incoming$" . font-lock-function-name-face)
     ("^ *[0-9]+" . font-lock-comment-face)
     ("\\[\\( [1-9]\\|[1-9][0-9]\\)\\] \\[[ X]\\] " 1 'github-mode-ok-face)
     ("\\[\\( 0\\)\\] \\[[ X]\\] " 1 'github-mode-warn-face)
     ("\\[[ 0-9]\\{2\\}\\] \\[\\(X\\)\\] " 1 'github-mode-ok-face)
     ("\\[[ X]\\] \\( XS\\|  S\\)" 1 'github-mode-ok-face)
     ("\\[[ X]\\] \\(  M\\|  L\\)" 1 'github-mode-warn-face)
     ("\\[[ X]\\] \\( XL\\|XXL\\)" 1 'github-mode-err-face)
     ;; highlight the enclosing brackets without changing anything else
     ("\\[[ 0-9]\\{2\\}\\] \\[[ X]\\] .+$" 0 font-lock-comment-face keep))
    t))

(defvar github-mode-pr-font-lock-defaults
  '((("^commit: \\(.*\\)$" 1 font-lock-type-face)
     ("^branch: \\(.*\\)$" 1 font-lock-preprocessor-face)
     ("^author: \\(.*\\)$" 1 font-lock-constant-face)
     ("| \\(\\+[ 0-9]+\\) -[ 0-9]+$" 1 'github-mode-ok-face)
     ("| \\+[ 0-9]+ \\(-[ 0-9]+\\)$" 1 'github-mode-err-face))
    t))

(define-derived-mode github-mode special-mode "github"
  "Major mode for github reviews in Emacs."
  (setq-local
   font-lock-defaults
   (let ((name (buffer-name)))
     (cond ((equal name gh-summary-buffer-name)
            github-mode-summary-font-lock-defaults)
           ((gh-is-pr-buffer name)
            github-mode-pr-font-lock-defaults)
           (t nil)))))



(defun gql-query (query-struct)
  "Generate a graphql query string for the given QUERY-STRUCT."
  (cond
   ((stringp query-struct)
    query-struct)
   ((listp query-struct)
    (let* ((node (car query-struct))
           (node-string
            (cond
             ((stringp node)
              node)
             ((listp node)
              (format "%s(%s)" (car node) (mapconcat 'identity (cdr node) ",")))))
           (elts (mapconcat 'identity (seq-map 'gql-query (cdr query-struct)) " ")))
      (format "%s { %s }" node-string elts)))))

(provide 'github)

;;; github.el ends here

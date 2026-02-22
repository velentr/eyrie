;;; org-lifelist.el --- Filter and view bucket lists.

;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-2.0-only
;;;
;;; Author: Brian Kubisiak <brian@kubisiak.com>

;;; Commentary:

;;; Code:

(require 'csv-mode)
(require 'org)

(defun org-lifelist--entry (level title done? leaf?)
  "Construct an entry at depth LEVEL and heading TITLE marked DONE? or LEAF?."
  (list level title done? leaf?))

(defun org-lifelist--entry->level (entry)
  "Get the level for ENTRY."
  (car entry))

(defun org-lifelist--entry->title (entry)
  "Get the title for ENTRY."
  (cadr entry))

(defun org-lifelist--entry->done? (entry)
  "Check whether ENTRY is done."
  (caddr entry))

(defun org-lifelist--entry->leaf? (entry)
  "Check whether ENTRY is a leaf."
  (cadddr entry))

(defun org-lifelist--prune-list (lst)
  "Prune LST to contain leaf entries and corresponding headings."
  (let ((to-check (reverse lst))
        (so-far nil))
    (dolist (this to-check)
      (let ((next (car so-far)))
        (if (or (org-lifelist--entry->leaf? this)
                (and next
                     (< (org-lifelist--entry->level this)
                        (org-lifelist--entry->level next))))
            (setq so-far (cons this so-far)))))
    so-far))

(defun org-lifelist--load-from-buffer (tag)
  "Load lifelist data from a buffer filtered by TAG."
  (org-map-entries
   (lambda ()
     (let ((components (org-heading-components)))
       (org-lifelist--entry
        (car components)
        (nth 4 components)
        (equal "DONE" (nth 2 components))
        (not (not (nth 2 components))))))
   (string-join (list tag "|-TODO=\"DONE\"-TODO=\"TODO\""))))

(defun org-lifelist--title-to-tag ()
  "Get a hash table mapping titles to tags in the current buffer."
  (let ((all-tags (org-get-buffer-tags))
        (title-to-tag (make-hash-table :test 'equal)))
    (dolist (tag-cell all-tags)
      (let* ((tag (car tag-cell))
             (keyword (upcase (string-join (list "lifelist_" tag))))
             (keyword-list (list keyword))
             (list-title
              (alist-get
               keyword
               (org-collect-keywords keyword-list keyword-list)
               tag
               nil
               #'equal)))
        (puthash list-title
                 tag
                 title-to-tag)))
    title-to-tag))

(defun org-lifelist-open (tag title)
  "Open a new lifelist view filtered by TAG titled TITLE."
  (interactive
   (let* ((title-to-tag (org-lifelist--title-to-tag))
          (title (completing-read "List to view: " title-to-tag)))
     (list (gethash title title-to-tag) title))
   org-mode)
  (let ((entries
         (org-lifelist--prune-list (org-lifelist--load-from-buffer tag)))
        (buf (get-buffer-create (string-join (list "*" title "*")))))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      (insert "#+title: " title "\n\n* " title " [/] [%]\n")
      (dolist (entry (cdr entries))
        (let ((level (org-lifelist--entry->level entry))
              (title (org-lifelist--entry->title entry))
              (leaf? (org-lifelist--entry->leaf? entry))
              (done? (org-lifelist--entry->done? entry)))
          (insert (make-string level ?*)
                  (if leaf?
                      (if done? " DONE " " TODO ")
                    " ")
                  title
                  "\n")))
      (goto-char (point-min))
      ;; Note that this is orders of magnitude faster than
      ;; (org-update-statistics-cookies t) but should be doing the same thing?
      (org-map-entries
       (lambda ()
         (org-update-statistics-cookies nil)))
      (read-only-mode))
    (switch-to-buffer buf)))

(defun org-lifelist--tag-entry (name tag)
  "Mark the entry NAME with TAG."
  (let ((header (org-find-exact-headline-in-buffer name)))
    (if (not header)
        nil
      (goto-char header)
      (org-set-tags
       (delete-dups (append (org-get-tags) (list tag))))
      t)))

(defun org-lifelist-tag-entries-from-buffer (org-buffer tag)
  "Tag entries in ORG-BUFFER with TAG matching the names in the current buffer."
  (interactive
   (let* ((buffer-name (read-buffer "Org buffer: "))
          (title-to-tag
           (with-current-buffer buffer-name (org-lifelist--title-to-tag)))
          (title (completing-read "Tag to use: " title-to-tag)))
     (list buffer-name (gethash title title-to-tag))))
  (goto-char (point-min))
  (while (not (eobp))
    (let ((name (buffer-substring
                 (line-beginning-position) (line-end-position))))
      (if (with-current-buffer org-buffer (org-lifelist--tag-entry name tag))
          (kill-whole-line)
        (forward-line 1)))))

(defun org-lifelist-tag-entries-from-csv (csv tag)
  "Tag entries in the current buffer matching CSV entries with TAG."
  (interactive
   (let* ((csv-file-name (expand-file-name (read-file-name "CSV file: ")))
          (title-to-tag (org-lifelist--title-to-tag))
          (title (completing-read "Tag to use: " title-to-tag)))
     (list csv-file-name (gethash title title-to-tag)))
   org-mode)
  (let ((org-buffer (current-buffer)))
    (switch-to-buffer "*csv-copy*")
    (insert-buffer (find-file-noselect csv))
    (csv-mode)
    (csv-kill-fields '(1 2 3 4 5 6 7 9 10) (point-min) (point-max))
    (goto-char (point-min))
    (kill-whole-line)
    (org-lifelist-tag-entries-from-buffer org-buffer tag)))

(provide 'org-lifelist)

;;; org-lifelist.el ends here

;;; aircam.el --- Minor modes for aircam development

;;; SPDX-FileCopyrightText: 2021 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-2.0-only

;;; Commentary:

;; Minor modes for aircam development

;;; Code:

(require 'company)
(require 'flycheck)

(defvar aircam-sources "~/aircam"
  "Path to the aircam source tree.")

(defvar aircam-tuple "aarch64-linux-gnu"
  "Compiler tuple to use for searching compile_commands.json database.

Generally should not be set manually, use aircam-set-target instead.")

(defconst aircam--target-to-tuple
  '(("workstation" . "x86_64-linux-gnu")
    ("vehicle"     . "aarch64-linux-gnu")
    ("beacon"      . "arm-linux-gnueabihf")
    ("joysticks"   . "arm-linux-gnueabihf")
    ("airhawk"     . "arm-none-eabi_stm32f4xx")
    ("gimbal"      . "arm-none-eabi_stm32f4xx")
    ("skytrain"    . "arm-none-eabi_stm32f0xx")
    ("handhawk"    . "arm-none-eabi_stm32f4xx"))
  "Mapping from dev target to compiler tuple.")

(defun aircam-set-target (target)
  "Set aircam-tuple based on the desired development TARGET."
  (interactive
   (list (completing-read "Aircam target: " aircam--target-to-tuple)))
  (setq
   aircam-tuple
   (assoc-default target aircam--target-to-tuple 'equal "aarch64-linux-gnu"))
  (aircam-cc-setup))

(defun aircam-read-suite ()
  "Read the Ubuntu suite version from /etc/lsb-release."
  (let ((lsb-release (find-file-noselect "/etc/lsb-release" t)))
    (with-current-buffer lsb-release
      (unwind-protect
          (if (search-forward "DISTRIB_CODENAME=" nil t)
              (thing-at-point 'word)
            "bionic")
        (kill-buffer lsb-release)))))

(defvar aircam-suite (aircam-read-suite)
  "Ubuntu Linux suite to use to search for include files.")

(defconst aircam-c-style
  '((c-basic-offset . 2)
    (c-offsets-alist . ((innamespace . 0)
                        (statement-cont . ++)
                        (brace-list-intro . ++)
                        (member-init-intro . ++)
                        (case-label . +)
                        (arglist-intro . ++)
                        (access-label . -1)))
    (c-hanging-braces-alist . ((substatement-open after))))
  "Aircam C++ programming style.")
(c-add-style "aircam" aircam-c-style nil)

(define-minor-mode aircam-c++-mode
  "Minor mode for working with aircam C++ code"
  :init-value nil
  :lighter " AircamC++"
  :keymap nil
  (cond
   (aircam-c++-mode
    (setq default-directory (concat aircam-sources "/"))
    (setq indent-tabs-mode nil)
    (c-set-style "aircam")
    (set-fill-column 100)
    (aircam-cc-setup))))

(define-minor-mode aircam-py-mode
  "Minor mode for working with aircam python code"
  :init-value nil
  :lighter " AircamPy"
  :keymap nil
  (cond
   (aircam-py-mode
    (setq default-directory (concat aircam-sources "/"))
    (setq indent-tabs-mode nil)
    (set-fill-column 100))))

(defun aircam-code-format ()
  "Run code_format on the current buffer."
  (interactive)
  (save-buffer)
  (call-process
   (concat aircam-sources "/skyrun")
   nil nil nil
   "bin" "code_format" buffer-file-name)
  (revert-buffer t t t))

(defvar aircam-compile-commands (make-hash-table :test 'equal)
  "Table of compile commands for aircam C++ code.")

(defun aircam--compile-commands-path (tuple)
  "Get the path to the compile_commands.json file for the given compiler TUPLE."
  (let ((suite (if (string-prefix-p "arm-" tuple)
                   "None"
                 aircam-suite))
        (type (if (string-prefix-p "x86_64-linux-" tuple)
                  "RelWithDebInfo"
                "Release"))
        (sources (expand-file-name aircam-sources)))
    (concat sources "/build/build/" suite "/" tuple "/" type
            "/aircam/compile_commands.json")))

(defun aircam--load-compile-commands (tuple)
  "Load the compilation commands database for TUPLE."
  (let* ((commands-file (aircam--compile-commands-path tuple))
         (commands-buffer (find-file-noselect commands-file t))
         (commands-list
          (with-current-buffer commands-buffer
            (unwind-protect
                (json-parse-buffer)
              (kill-buffer commands-buffer))))
         (commands (make-hash-table :test 'equal)))
    (seq-do (lambda (command)
              (puthash
               (gethash "file" command)
               (cons (gethash "command" command)
                     (gethash "directory" command))
               commands))
            commands-list)
    commands))

(defun aircam--fuzzy-get-cc (filename ccdb)
  "Get the cc data for FILENAME from CCDB, falling back to a .cc file."
  (let ((ccfilename (concat (substring filename nil -2) ".cc"))
        (ccdbentry (gethash filename ccdb (cons "" ""))))
    (cond
     ((and (equal (car ccdbentry) "")
           (string-suffix-p ".h" filename))
      (gethash ccfilename ccdb (cons "" "")))
     (t ccdbentry))))

(defun aircam--get-cc ()
  "Get the compiler command data for the current tuple for building the buffer."
  (let* ((existing-cc (gethash aircam-tuple aircam-compile-commands))
         (ccdb (if existing-cc
                   existing-cc
                 (puthash aircam-tuple
                          (aircam--load-compile-commands aircam-tuple)
                          aircam-compile-commands)))
         (filename (file-truename buffer-file-name))
         (cc-cons (aircam--fuzzy-get-cc filename ccdb))
         (cc (split-string (car cc-cons)))
         (dir (cdr cc-cons)))
    (cons cc dir)))

(defun aircam--cc-parsearg (cc prefix)
  "Parse out all args in CC starting with PREFIX."
  (let* ((divided
          (seq-group-by (lambda (arg) (string-prefix-p prefix arg)) cc))
         (matches (cdr (assoc 't divided)))
         (remaining (cdr (assoc nil divided))))
    (cons
     (seq-map (lambda (match) (substring match (length prefix))) matches)
     remaining)))

(defun aircam--cc-warnings (cc)
  "Get the warning options (-Wxxx) from the given CC command."
  (aircam--cc-parsearg cc "-W"))

(defun aircam--cc-defines (cc)
  "Get the macro definitions from the given CC command."
  (aircam--cc-parsearg cc "-D"))

(defun aircam--cc-fix-relpath (relpath directory)
  "Fix RELPATH from the ccdb to be absolute in DIRECTORY in aircam-sources."
  (expand-file-name relpath directory))

(defun aircam--cc-includes (cc directory)
  "Get the include paths from the given CC command in the given DIRECTORY."
  (let* ((parsed (aircam--cc-parsearg cc "-I"))
         (remaining (cdr parsed))
         (includes (car parsed)))
    (cons
     (seq-map (lambda (include)
                (aircam--cc-fix-relpath include directory))
              includes)
     remaining)))

(defun aircam--cc-std (cc)
  "Get the language standard from the given CC command."
  (let* ((parsed (aircam--cc-parsearg cc "-std="))
         (remaining (cdr parsed))
         (std (caar parsed)))
    (cons std remaining)))

(defun aircam--cc-remaining-args (cc dir &optional sofar)
  "Filter the remaining useful compiler args from CC in DIR, appending SOFAR."
  (if cc
      (let ((arg (car cc))
            (next (cdr cc)))
        (cond
         ;; TODO --sysroot is still broken with clang
         ;; ((string-prefix-p "--sysroot=" arg)
         ;;  (aircam--cc-remaining-args next dir (cons arg sofar)))
         ((equal "-isystem" arg)
          (aircam--cc-remaining-args (cdr next)
                                     dir
                                     (cons (aircam--cc-fix-relpath
                                            (car next)
                                            dir)
                                           (cons arg sofar))))
         (t (aircam--cc-remaining-args next dir sofar))))
    (reverse sofar)))

(defun aircam--cc-filter-warnings (warnings)
  "Filter WARNINGS to remove anything that clang does not support (and -Werror)."
  (seq-filter
   (lambda (warning)
     (not (or (equal warning "no-maybe-uninitialized")
              (equal warning "no-terminate")
              ;; Remove -Werror as well so flycheck can correctly distinguish
              ;; between warnings and errors.
              (equal warning "error"))))
   warnings))

(defun aircam-setup-flycheck (&optional cached-cc directory)
  "Set the flycheck custom variables using CACHED-CC compile command in DIRECTORY."
  (interactive)
  (let* ((cc
          (if cached-cc cached-cc
            (car (aircam--get-cc))))
         (dir
          (if directory directory
            (cdr (aircam--get-cc))))
         (cc-defines (aircam--cc-defines cc))
         (definitions (car cc-defines))
         (cc-includes (aircam--cc-includes (cdr cc-defines) dir))
         (includes (car cc-includes))
         (cc-warnings (aircam--cc-warnings (cdr cc-includes)))
         (warnings (aircam--cc-filter-warnings (car cc-warnings)))
         (cc-std (aircam--cc-std (cdr cc-warnings)))
         (std (car cc-std))
         (cc-extra (aircam--cc-remaining-args (cdr cc-std) dir)))
    (custom-set-variables
     '(flycheck-gcc-language-standard std)
     '(flycheck-clang-language-standard std)
     '(flycheck-gcc-include-path includes)
     '(flycheck-clang-include-path includes)
     ;; TODO: add -isystem paths as well
     '(flycheck-cppcheck-include-path includes)
     '(flycheck-gcc-warnings warnings)
     '(flycheck-clang-warnings warnings)
     '(flycheck-gcc-definitions definitions)
     '(flycheck-clang-definitions definitions)
     '(flycheck-gcc-args cc-extra)
     '(flycheck-clang-args cc-extra))
    (flycheck-buffer)))

(defun aircam-setup-company (&optional cached-cc directory)
  "Set the company clang backend custom vars using CACHED-CC in DIRECTORY."
  (interactive)
  (company-mode t)
  (setq company-backends (delete 'company-semantic company-backends))
  ;; TODO: de-dup this with aircam-setup-flycheck
  (let* ((cc
          (if cached-cc cached-cc
            (car (aircam--get-cc))))
         (dir
          (if directory directory
            (cdr (aircam--get-cc))))
         (cc-defines (aircam--cc-defines cc))
         (definitions
           (seq-map
            (lambda (def)
              (concat "-D" def))
           (car cc-defines)))
         (cc-includes (aircam--cc-includes (cdr cc-defines) dir))
         (includes
          (seq-map
           (lambda (inc)
             (concat "-I" inc))
           (car cc-includes)))
         (cc-std (aircam--cc-std (cdr cc-includes)))
         (std (list (concat "-std=" (car cc-std))))
         (cc-extra (aircam--cc-remaining-args (cdr cc-std) dir)))
    (custom-set-variables
     '(company-clang-arguments (append includes definitions std cc-extra)))))

(defun aircam-cc-setup ()
  "Set up aircam C++ code based on the cc db."
  (interactive)
  (let* ((cc-cons (aircam--get-cc))
         (cc (car cc-cons))
         (dir (cdr cc-cons)))
    (if (not cc)
        '()
      (aircam-setup-flycheck cc dir)
      (aircam-setup-company cc dir))))

(provide 'aircam)

;;; aircam.el ends here

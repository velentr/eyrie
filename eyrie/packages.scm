;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:export (emacs-color-theme-solarized
            emacs-github-mode
            emacs-worklog
            git-third-party))

(define git-third-party
  (package
   (name "git-third-party")
   (version "0.0.0")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/velentr/git-third-party")
       (commit (string-append "v" version))))
     (sha256
      (base32 "1mgrs047mgl0s243a3y9xqgl75ax5b2x45zhccyi1vq0s5nxhqsl"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-clap" ,rust-clap-2))))
   (propagated-inputs
    `(("git" ,git)))
   (home-page "https://github.com/velentr/git-third-party")
   (synopsis "Manage vendored third-party code in git")
   (description
    "Manage third-party code that is vendored into a git monorepo.")
   (license license:gpl3)))

(define emacs-worklog
  (package
   (name "emacs-worklog")
   (version "0")
   (source (local-file "./packages/worklog" #:recursive? #t))
   (build-system emacs-build-system)
   (home-page
    "https://github.com/velentr/eyrie/tree/master/eyrie/packages/worklog")
   (synopsis "Track work across projects in emacs")
   (description
    "Worklogs are a way of tracking which projects are active and what work
you’ve already completed or have left to do.  This project contains emacs code
for creating and managing worklogs to manage your projects and keep you focused
on driving projects to completion.")
   (license license:gpl2)))

(define emacs-github-mode
  (package
   (name "emacs-github-mode")
   (version "0")
   (source (local-file "./packages/github-mode" #:recursive? #t))
   (build-system emacs-build-system)
   (home-page
    "https://github.com/velentr/eyrie/tree/master/eyrie/packages/github-mode")
   (synopsis "Interact with github in emacs")
   (description
    "Make up for github's poor review interface by doing some interaction from
emacs.")
   (license license:gpl2)))

(define emacs-color-theme-solarized
  ;; From 2017-10-24.
  ;; No releases available.
  (let ((commit "f3ca8902ea056fb8e46cb09f09c96294e31cd4ee") (revision "0"))
    (package
      (name "emacs-color-theme-solarized")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url
                       "https://github.com/sellout/emacs-color-theme-solarized")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "16d7adqi07lzzr0qipl1fbag9l8kiyr3xrqxi528pimcisbg85d3"))))
      (build-system emacs-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 ;; these are intended for old versions of emacs and do not
                 ;; compile with emacs>=24
                 (add-before 'install 'remove-color-theme
                   (lambda _
                     (delete-file "./color-theme-solarized.el")
                     (delete-file "./color-theme-solarized-pkg.el"))))))
      (home-page "https://github.com/sellout/emacs-color-theme-solarized")
      (synopsis "Solarized Colorscheme for Emacs")
      (description
       "Emacs highlighting using Ethan Schoonover’s Solarized color scheme.")
      (license license:expat))))

;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system cargo)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:export (git-third-party-ey))

(define git-third-party-ey
  (package
   (name "git-third-party-ey")
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

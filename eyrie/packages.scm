;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system rebar)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:export (emacs-color-theme-solarized
            emacs-github-mode
            emacs-worklog
            erlang-cowboy
            erlang-cowlib
            erlang-ranch
            git-third-party
            knowledge-store
            ytar))

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

(define knowledge-store
  (package
    (name "knowledge-store")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/velentr/ks")
         (commit (string-append "v" version))))
       (sha256
        (base32 "0dk13nnprm1kpzzn1nnb2d25205p0q55rxhnq5qhm9crqaxxiv7n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target)))
       #:tests? #f  ;; TODO: this requires splint, which is not upstream
       #:phases
       (modify-phases
           %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1"))
                    (zsh (string-append out "/share/zsh/site-functions")))
               (install-file "ks" bin)
               (install-file "ks.1" man)
               (install-file "zsh/_ks" zsh)))))))
    (native-inputs
     (list asciidoc pkg-config ragel))
    (inputs
     `(("sqlite" ,sqlite)))
    (home-page "https://github.com/velentr/ks")
    (synopsis "CLI knowledge store")
    (description
     "Ks is a simple, CLI-based document library that stores documents together
with metadata in a single SQLite database. Though intended to store PDFs, ks
makes no assumption about data format and may be used to store any type of
document.")
    (license license:expat)))

(define ytar
  (package
   (name "ytar")
   (version "0")
   (source (local-file "./packages/ytar" #:recursive? #t))
   (build-system copy-build-system)
   (arguments
    (list #:phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'patch-file-paths
                (lambda* (#:key inputs #:allow-other-keys)
                  (substitute* "ytar"
                    (("yt-dlp")
                     (search-input-file inputs "/bin/yt-dlp"))))))
          #:install-plan #~'(("ytar" "bin/"))))
   (inputs (list yt-dlp))
   (home-page
    "https://github.com/velentr/eyrie/tree/master/eyrie/packages/ytar")
   (synopsis "Archive youtube video playlists")
   (description
    "Download video playlists from youtube, keeping track of which videos are
new based on symlinks.")
   (license license:expat)))

(define erlang-ranch
  (package
    (name "erlang-ranch")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "ranch" version))
       (sha256
        (base32 "1rfz5ld54pkd2w25jadyznia2vb7aw9bclck21fizargd39wzys9"))))
    (build-system rebar-build-system)
    (home-page "https://ninenines.eu/docs/#ranch")
    (synopsis "Socket acceptor pool for TCP protocols")
    (description "Ranch is a socket acceptor pool for TCP protocols.  Ranch aims
to provide everything you need to accept TCP connections with a small code base
and low latency while being easy to use directly as an application or to embed
into your own.")
    (license license:isc)))

(define erlang-cowlib
  (package
    (name "erlang-cowlib")
    (version "2.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "cowlib" version))
       (sha256
        (base32 "1ac6pj3x4vdbsa8hvmbzpdfc4k0v1p102jbd39snai8wnah9sgib"))))
    (build-system rebar-build-system)
    (arguments
     '(#:tests? #f))
    (home-page "https://ninenines.eu/docs/#cowlib")
    (synopsis "Support library for manipulating Web protocols")
    (description "Cowlib is a support library for manipulating Web protocols.
Cowlib provides libraries for parsing and building messages for various Web
protocols, including HTTP/1.1, HTTP/2 and Websocket.")
    (license license:isc)))

(define erlang-cowboy
  (package
    (name "erlang-cowboy")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (hexpm-uri "cowboy" version))
       (sha256
        (base32 "1phv0a1zbgk7imfgcm0dlacm7hbjcdygb0pqmx4s26jf9f9rywic"))))
    (build-system rebar-build-system)
    (propagated-inputs
     (list erlang-cowlib erlang-ranch))
    (home-page "https://ninenines.eu/docs/#cowboy")
    (synopsis "Small, fast, modern HTTP server for Erlang/OTP")
    (description "Cowboy is a small, fast and modern HTTP server for Erlang/OTP.
Cowboy aims to provide a complete HTTP stack in a small code base.")
    (license license:isc)))

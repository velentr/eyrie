;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages man)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages rails)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (guix build utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system rebar)
  #:use-module (guix build-system ruby)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:export (birdr
            emacs-github-mode
            emacs-worklog
            emacs-evil-textobj-tree-sitter
            emacs-combobulate
            g-hooks
            git-third-party
            kitchen
            knowledge-store
            magpie
            magpie-plugins
            python-async-lru
            revup
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
     (list #:cargo-inputs
           `(("rust-clap" ,rust-clap-2))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-file-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* '("src/cherrypick.rs" "src/clone.rs")
                     (("\"git\"")
                      (string-append
                       "\""
                       (search-input-file inputs "/bin/git")
                       "\""))))))))
    (inputs (list git))
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
   (propagated-inputs (list emacs-ghub))
   (home-page
    "https://github.com/velentr/eyrie/tree/master/eyrie/packages/github-mode")
   (synopsis "Interact with github in emacs")
   (description
    "Make up for github's poor review interface by doing some interaction from
emacs.")
   (license license:gpl2)))

(define emacs-evil-textobj-tree-sitter
  ;; From 2023-08-16
  ;; No releases available
  (let ((commit "19979843f5fc437917f9a4dae977f5e6d4793726")
        (revision "0"))
    (package
      (name "emacs-evil-textobj-tree-sitter")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/meain/evil-textobj-tree-sitter")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1mqh6zqjnbdmqblqpv1409rmx7h1wqprp1z7h68mvmyl80c8r6bd"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'install-pieces
              (lambda _
                (let ((treesit-queries
                       (string-append #$output "/share/emacs/site-lisp/evil-textobj-tree-sitter-"
                                      #$version "/treesit-queries")))
                  (mkdir-p treesit-queries)
                  (copy-recursively "treesit-queries" treesit-queries)))))))
      (home-page "https://github.com/meain/evil-textobj-tree-sitter")
      (synopsis "Tree-sitter powered textobjects for evil mode in Emacs")
      (description "This package will let you create evil textobjects using
tree-sitter grammars. You can easily create @code{function}, @code{class},
@code{comment} etc textobjects in multiple languages.")
      (license license:asl2.0))))

(define emacs-combobulate
  ;; From 2023-09-25
  ;; No releases available
  (let ((commit "c7e4670a3047c0b58dff3746577a5c8e5832cfba")
        (revision "0"))
    (package
      (name "emacs-combobulate")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mickeynp/combobulate")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "063w2sm0c7xhg3ml31xp870azb0sv7z689lnbnjnbl3rfdy4kg50"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/mickeynp/combobulate")
      (synopsis "Structured Editing and Navigation in Emacs")
      (description "Combobulate is a package that adds structured editing and
movement to a wide range of programming languages.")
      (license license:gpl3))))

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

(define python-async-lru
  (package
    (name "python-async-lru")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "async-lru" version))
       (sha256
        (base32 "09sn3bc3gc2flijm9k8kn4hmbnlkaddhqahb49izy188yrfrm9dq"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-typing-extensions))
    (home-page "https://github.com/aio-libs/async-lru")
    (synopsis "Simple LRU cache for asyncio")
    (description
     "This package is 100% port of Python built-in function
functools.lru_cache for asyncio.")
    (license license:expat)))

(define revup
  (package
    (name "revup")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "revup" version))
       (sha256
        (base32 "14vmjlkrq29xjiyqhc50j1w73xff01v6i953p6hliri6bvili80i"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f  ;; revup doesn't have any tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tool-inputs
            (lambda _
              (substitute* "revup/revup.py"
                (("man_cmd = \\(\"man")
                 (string-append "man_cmd = (\""
                                (which "man"))))
              (substitute* "revup/git.py"
                (("shutil.which\\(\"git\"\\)")
                 (string-append "\"" (which "git") "\""))))))))
    (inputs (list git man-db))
    (propagated-inputs (list python-aiohttp python-async-lru python-rich))
    (native-inputs (list python-wheel))
    (home-page "https://github.com/Skydio/revup")
    (synopsis "Revolutionary commit-based code review and workflow tools for
git/github")
    (description "Revup provides command-line tools that allow developers to
iterate faster on parallel changes and reduce the overhead of creating and
maintaining code reviews.")
    (license license:expat)))

(define kitchen
  (package
    (name "kitchen")
    (version "0.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/velentr/kitchen")
         (commit (string-append "v" version))))
       (sha256
        (base32 "1v3i15fgq24dnp0jrkszzvn8i8b3w2k5sxibkw1fzic0lqippp22"))))
    (build-system ruby-build-system)
    (arguments
     (list #:tests? #f    ;; I haven't written any real tests yet
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-file-paths
                 (lambda _
                   (substitute* '("bin/kitchen")
                     (("'puma'")
                      (string-append "'" (which "puma") "'"))))))))
    (propagated-inputs
     (list ruby-2.7 ruby-byebug ruby-listen ruby-kramdown ruby-puma ruby-rails
           ruby-sqlite3 ruby-tzinfo-data))
    (synopsis "Personal recipe tracker")
    (description "Kitchen is a personal recipe/cooking tracker that aims to be a
self-hosted way of collecting your favorite recipes and how often you cook
them.")
    (home-page "https://github.com/velentr/kitchen")
    (license license:expat)))

(define magpie
  (package
    (name "magpie")
    (version "0.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/velentr/magpie")
         (commit "76a26a07cd3ed72b982d359285db1786f15feb5a")))
       (sha256
        (base32 "0000adrv0k3pg9rk8zj0f5pyf72srhkbbqpj5qjv98alspgccyvn"))))
    (build-system copy-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-input-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (engines (apply
                              append
                              (map (lambda (engine)
                                     (list
                                      (string-append bin "/magpie-" engine "-init")
                                      (string-append bin "/magpie-" engine "-sync")))
                                   '("git" "rsync"))))
                    (PATH
                     (map (lambda (package)
                            (string-append (assoc-ref inputs package) "/bin"))
                          '("coreutils"
                            "diffutils"
                            "findutils"
                            "git"
                            "grep"
                            "rsync"
                            "sed"))))
               (for-each
                (lambda (script)
                  (wrap-program script
                    `("PATH" ":" prefix ,PATH)))
                engines)
               (wrap-program (string-append bin "/magpie")
                 `("PATH" ":" prefix (,bin)))))))
       #:install-plan '(("magpie" "bin/magpie")
                        ("magpie-git-init" "bin/magpie-git-init")
                        ("magpie-git-sync" "bin/magpie-git-sync")
                        ("magpie-rsync-init" "bin/magpie-rsync-init")
                        ("magpie-rsync-sync" "bin/magpie-rsync-sync"))))
    (inputs
     (list coreutils diffutils findutils git grep python rsync sed))
    (synopsis "Simple scriptable backup script")
    (description "Magpie is a simple backup script that treats data as a set of
channels. Each channel is synchronized with a remote using an engine. Magpie is
designed to be easily extensible to add new engines with different
capabilities.")
    (home-page "https://github.com/velentr/magpie")
    (license license:gpl3)))

(define magpie-plugins
  ;; There is not yet a stable release
  (let ((commit "16efa237599bafbb0e71e070c648d642663345b4")
        (revision "0"))
    (package
      (name "magpie-plugins")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/velentr/magpie-plugins")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qdjxjp5fclcf4qzmizg1zc6wfd8vpz4y7r76lm00yw0rhgbgmw5"))))
      (build-system cargo-build-system)
      (arguments
       (list #:cargo-inputs
             `(("rust-ciborium" ,rust-ciborium-0.2)
               ("rust-env-logger" ,rust-env-logger-0.9)
               ("rust-failure" ,rust-failure-0.1)
               ("rust-log" ,rust-log-0.4)
               ("rust-serde" ,rust-serde-1)
               ("rust-serde-bytes" ,rust-serde-bytes-0.11)
               ("rust-xdg" ,rust-xdg-2))
             #:install-source? #f))
      (propagated-inputs
       (list rsync))
      (home-page "https://github.com/velentr/magpie-plugins")
      (synopsis "More complex magpie engines, based on CRDT concepts")
      (description "Additional plugins that translate filesystem layouts into
CRDTs for sync'ing with a remote server using rsync.")
      (license license:gpl3))))

(define birdr
  (package
    (name "birdr")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/velentr/birdr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12yd371pzlf0aj417ig42my6cjrylsdk1vap9y3lm8wk3qzxyp54"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #false))
    (propagated-inputs
     (list python-click python-rich python-sqlalchemy))
    (synopsis "Record and track bird sightings and checklists")
    (description "Command-line tool for tracking bird sightings and checklists in a
database.")
    (home-page "https://github.com/velentr/birdr")
    (license license:gpl3)))

(define g-hooks
  (package
    (name "g-hooks")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/velentr/g-hooks")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bd6rwgrla9is8bwxim9clys0bn359fzzkpb2845hchdh3vxwzlk"))))
    (build-system guile-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-tool-inputs
                 (lambda _
                   (substitute* "g-hooks/core.scm"
                     (("define %git \"git\"")
                      (string-append "define %git \"" (which "git") "\"")))))
               (add-after 'install 'wrap-program
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((bin (string-append #$output "/bin"))
                          (bytestructures
                           (assoc-ref inputs "guile-bytestructures"))
                          (gcrypt (assoc-ref inputs "guile-gcrypt"))
                          (git (assoc-ref inputs "guile-git"))
                          (guix (assoc-ref inputs "guix"))
                          (version (target-guile-effective-version))
                          (scm (string-append "/share/guile/site/" version))
                          (go (string-append
                               "/lib/guile/"
                               version
                               "/site-ccache")))
                     (install-file "scripts/g-hooks" bin)
                     (wrap-program (string-append bin "/g-hooks")
                       '("GUILE_AUTO_COMPILE" prefix
                         ("0"))
                       ;; TODO: fix this nonsense
                       `("GUILE_LOAD_PATH" prefix
                         (,(string-append #$output scm)
                          ,(string-append bytestructures scm)
                          ,(string-append gcrypt scm)
                          ,(string-append git scm)
                          ,(string-append guix scm)))
                       `("GUILE_LOAD_COMPILED_PATH" prefix
                         (,(string-append #$output go)
                          ,(string-append bytestructures go)
                          ,(string-append gcrypt go)
                          ,(string-append git go)
                          ,(string-append guix go))))))))))
    (native-inputs (list guile-3.0))
    (inputs
     (list bash-minimal git guile-bytestructures guile-gcrypt guile-git guix))
    (synopsis "Manage git hooks using guix")
    (description "g-hooks allows you to describe git hooks using guix’s
g-expressions, then build and install them as a private profile under
.git/g-hooks.")
    (home-page "https://github.com/velentr/g-hooks")
    (license license:gpl3)))

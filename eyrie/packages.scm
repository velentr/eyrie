;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
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
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xml)
  #:use-module (guix build utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system meson)
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
            emacs-org-lifelist
            emacs-evil-textobj-tree-sitter
            emacs-combobulate
            git-third-party
            kitchen
            knowledge-store
            magpie
            magpie-plugins
            python-async-lru
            python-garmin-fit-sdk
            rauc
            revup
            scripts
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

(define emacs-org-lifelist
  (package
   (name "emacs-org-lifelist")
   (version "0")
   (source (local-file "./packages/org-lifelist" #:recursive? #t))
   (build-system emacs-build-system)
   (home-page
    "https://github.com/velentr/eyrie/tree/master/eyrie/packages/org-lifelist")
   (synopsis "Track and filter lifelists with org")
   (description
    "Track and filter lifelists with org.")
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

(define scripts
  (package
    (name "scripts")
    (version "0")
    (source (local-file "./packages/scripts" #:recursive? #t))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-defusedxml python-garmin-fit-sdk python-pillow python-rich))
    (home-page "https://github.com/velentr/eyrie")
    (synopsis "Misc scripts for uncommon tasks")
    (description
     "This package bundles together a bunch of scripts that I use sometimes that
don't deserve their own packages.")
    (license license:gpl3)))

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
    (native-inputs (list python-setuptools python-wheel))
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
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "revup" version))
       (sha256
        (base32 "00877a4gdq6xd85kmmhi9crxbsmkdfx3iawldvb0pq4wfmr93d1f"))))
    (build-system pyproject-build-system)
    (arguments
     (list
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
                 (string-append "\"" (which "git") "\"")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python" "-m" "pytest")))))))
    (inputs (list git man-db))
    (propagated-inputs
     (list
      python-aiohttp
      python-aiosignal
      python-async-lru
      python-async-timeout
      python-charset-normalizer
      python-multidict
      python-requests
      python-rich
      python-yarl))
    (native-inputs
     (list
      python-mock
      python-pytest
      python-pytest-mock
      python-setuptools
      python-wheel))
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
  (let ((commit "53a16842ac9c211102b746c2766d728a03d6291f")
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
          (base32 "0njh7fr16h6l95jq2a1j8bnirxa31jkbywvhyl2my2sjfa7gj1lq"))))
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
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/velentr/birdr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18r3yfp2a7azhkmma4f0i1grpwzl7bj3mnbfm52j8hf09y5hsvag"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #false))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-click python-rich python-sqlalchemy))
    (synopsis "Record and track bird sightings and checklists")
    (description "Command-line tool for tracking bird sightings and checklists in a
database.")
    (home-page "https://github.com/velentr/birdr")
    (license license:gpl3)))

(define python-garmin-fit-sdk
  (package
    (name "python-garmin-fit-sdk")
    (version "21.133.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "garmin-fit-sdk" version))
       (sha256
        (base32 "104kzaqrcfa26rvxwzh1kllh5sir78fvl3wwi5pi8ljmxi50v939"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest python-pytest-mock python-setuptools python-wheel))
    (home-page "https://developer.garmin.com/fit/overview/")
    (synopsis "Official Garmin FIT Python SDK")
    (description
     "The Flexible and Interoperable Data Transfer (FIT) protocol is designed
specifically for the storing and sharing of data that originates from sport,
fitness and health devices. The FIT protocol defines a set of data storage
templates (FIT messages) that can be used to store information such as activity
data, courses, and workouts. It is designed to be compact, interoperable and
extensible.")
    ;; No license is provided?
    (license license:expat)))

(define rauc
  (package
    (name "rauc")
    (version "1.11.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/rauc/rauc/releases/download/v"
                       version "/rauc-" version ".tar.xz"))
       (sha256
        (base32 "045w82v3jhnrq34yd52rjvn3qzs9jvx9lr20zlj3mh1r552yx0pg"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(let* ((out #$output)
               (dbus-data (string-append out "/share/dbus-1"))
               (dbusinterfaces (string-append dbus-data "/interfaces"))
               (dbussystemservice (string-append dbus-data "/system-services")))
          (list
           (string-append "-Ddbusinterfacesdir=" dbusinterfaces)
           (string-append "-Ddbussystemservicedir=" dbussystemservice)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tests
            (lambda _
              (substitute* "test/meson.build"
                (("tests \\+= 'network'") ""))
              (substitute* "test/common.c"
                (("/sbin/mkfs.ext4") (which "mkfs.ext4")))
              (substitute* "test/test-dummy-handler.conf"
                (("/bin/echo") (which "echo")))))
          (add-after 'install 'wrap-rauc
            (lambda _
              (wrap-program (string-append #$output "/bin/rauc")
                `("PATH" ":" prefix
                  (,(string-append #$squashfs-tools "/bin")
                   ;; for mount and unmount
                   ,(string-append #$util-linux "/bin")
                   ;; for flash_erase, nandwrite, flashcp, and mkfs.ubifs
                   ,(string-append #$mtd-utils "/sbin")
                   ,(string-append #$tar "/bin")
                   ,(string-append #$e2fsprogs "/sbin")
                   ,(string-append #$dosfstools "/sbin")))))))))
    (native-inputs
     (list e2fsprogs `(,glib "bin") pkg-config python squashfs-tools))
    (inputs
     (list curl dbus glib json-glib libnl openssl))
    (synopsis "Safe and secure software updates for embedded Linux")
    (description "RAUC is a lightweight update client that runs on your Embedded
Linux device and reliably controls the procedure of updating your device with a
new firmware revision. RAUC is also the tool on your host system that lets you
create, inspect and modify update artifacts for your device.")
    (home-page "https://rauc.io")
    (license license:lgpl2.1)))

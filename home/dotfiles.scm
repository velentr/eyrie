(use-modules
  (gnu home)
  (gnu home services)
  (gnu packages)
  (gnu services)
  (guix gexp)
  (gnu home services shells)
  (ice-9 popen)
  (ice-9 rdelim)
  (ice-9 regex))

(define (stat-is-type? path type)
  (let ((st (stat path #f)))
    (and st (eq? (stat:type st) type))))
(define (is-directory? path)
  (stat-is-type? path 'directory))
(define (is-file? path)
  (stat-is-type? path 'regular))

(define (make-use flag tst)
  (cons flag tst))
(define (use-flag use)
  (car use))
(define (use-test use)
  (cdr use))

(define (read-next-flag port so-far)
  (let ((line (read-line port)))
    (if (eof-object? line)
        so-far
        (read-next-flag port (cons
                              (string->symbol line)
                              so-far)))))
(define home-use-flags
  (let ((home-use-filename (string-append (getenv "HOME") "/.guix-using")))
    (if (is-file? home-use-filename)
        (read-next-flag (open-input-file home-use-filename) '())
        '())))

(define use-flags
  (let ((available-use-flags
         (list
          (make-use 'always #t)
          (make-use 'documents
                    (lambda () (memq 'documents home-use-flags)))
          (make-use 'nix
                    (lambda () (is-file?
                                (string-append
                                 (getenv "HOME")
                                 "/.nix-profile/etc/profile.d/nix.sh"))))
          (make-use 'not-skydio
                    (lambda () (not (is-directory? "/home/skydio"))))
          (make-use 'personal
                    (lambda () (memq 'personal home-use-flags)))
          (make-use 'toolchain
                    (lambda () (memq 'toolchain home-use-flags)))
          (make-use 'skydio
                    (lambda () (is-directory? "/home/skydio")))
          (make-use 'x
                    (lambda () (memq 'x home-use-flags))))))
    (let ((use-exprs
           (filter (lambda (use)
                     (let ((tst (use-test use)))
                       (if (procedure? tst)
                           (tst)
                           tst)))
                   available-use-flags)))
      (map car use-exprs))))

(define (using? flag)
  (memq flag use-flags))

(define (filter-by-using modules)
  (map cdr
       (filter
        (lambda (module)
          (using? (car module)))
        modules)))

(define (flatfilter-by-using modules)
  (define (flatten so-far components)
    (if (null? components)
        so-far
        (flatten (append so-far (car components))
                 (cdr components))))
  (flatten '() (filter-by-using modules)))

(define %display-dpi
  (if (using? 'x)
      (let ((re (make-regexp "resolution: +([0-9]+)x[0-9]+ dots per inch"))
            (port (open-input-pipe "xdpyinfo")))
        (define (find-dpi)
          (let ((line (read-line port)))
            (if (eof-object? line)
                #f
                (let ((match (regexp-exec re line)))
                  (if match
                      (string->number (match:substring match 1))
                      (find-dpi))))))
        (let ((dpi (find-dpi)))
          (close-pipe port)
          (if dpi
              dpi
              (error "can't find dpi with xdpyinfo!"))))
      0))

(define %font-size
  (quotient (* 2 %display-dpi) 13))

(define %use-env
  (cons
   ;; add the use flags to the env, for double-checking when we are running
   (cons "GUIX_USING" (string-append
                       "'"
                       (string-join (map symbol->string use-flags) " ")
                       "'"))
   (flatfilter-by-using
    '((always . (("EDITOR" . "emacs")
                 ("HISTFILE" . "~/.histfile")
                 ("HISTSIZE" . "10000")
                 ("REPORTTIME" . "5")
                 ("SAVEHIST" . "10000")))
      (skydio . (("PYTHONDONTWRITEBYTECODE" . "1")        ;; don't clutter up aircam with .pyc
                 ("AIRCAM_ROOT" . "/home/skydio/aircam")  ;; default to aircam1
                 ("SKYCC_LOCAL_JOBS" . "10")))))))        ;; more parallelism

(define %use-packages
  (flatfilter-by-using
   '((always . ("bc"
                "clang"  ;; for emacs-company autocompletion
                "emacs-bazel"
                "emacs-cmake-mode"
                "emacs-company"
                "emacs-dts-mode"
                "emacs-erlang"
                "emacs-evil"
                "emacs-flycheck"
                "emacs-go-mode"
                "emacs-haskell-mode"
                "emacs-julia-mode"
                "emacs-lua-mode"
                "emacs-nix-mode"
                "emacs-no-x"
                "emacs-org"
                "emacs-rust-mode"
                "emacs-systemd-mode"
                "emacs-yaml-mode"
                "erlang"
                "file"
                "ghc"
                "ghc-hunit"
                "glibc-locales"
                "guile"
                "htop"
                "julia"
                "lsof"
                "lua"
                "ncdu"
                "nmap"
                "nss-certs"
                "password-store"
                "picocom"
                "psmisc"
                "reuse"
                "strace"
                "tree"
                "usbutils"
                "valgrind"
                "vim"
                "zsh"))
     (documents . ("ghostscript"
                   "texlive"))
     (personal . ("ledger"))
     (not-skydio . ("git"))     ;; revup uses a custom git
     (toolchain . ("gcc-toolchain"  ;; required by ghc
                   "gdb"))
     (x . ("feh"
           "font-adobe-source-code-pro"
           "fontconfig"
           "i3-wm"
           "i3status"
           "mpv"
           "rofi"
           "rxvt-unicode"
           "xdpyinfo"
           "xrdb")))))

(define (git-config)
  (define (config-option option)
    (string-append "\t" (car option) " = " (cadr option)))
  (define (config-section section)
    (let ((header (string-append "[" (car section) "]"))
          (body (map config-option (cadr section))))
      (string-join (cons header body) "\n")))
  (let ((config-tree
         (flatfilter-by-using
          '((always .   (("alias"      (("locate"     "ls-files --")
                                        ("ref"        "rev-parse HEAD")
                                        ("st"         "status --short")
                                        ("today"      "diff --stat '@{00:00:00}'")
                                        ("undo"       "checkout --")
                                        ("unstage"    "reset HEAD --")))
                         ("credential" (("helper"     "cache")))
                         ("color"      (("ui"         "true")))
                         ("core"       (("autocrlf"   "input")
                                        ("pager"      "less -F -X")))
                         ("grep"       (("lineNumber" "true")))
                         ("push"       (("default"    "simple")))
                         ("pull"       (("ff"         "only")))
                         ("rebase"     (("autosquash" "1")))
                         ("user"       (("name"       "Brian Kubisiak")))))
            (personal . (("user"       (("email"      "brian@kubisiak.com")))))
            (skydio .   (("alias"      (("ac"         "log --first-parent")))
                         ("user"       (("email"      "brian.kubisiak@skydio.com")))))))))
    (string-join (map config-section config-tree) "\n")))

(define (zsh-aliases)
  (define (make-alias alias)
    (let ((name (car alias))
          (value (cadr alias)))
      (string-append "alias " name "=" value)))
  (let ((aliases
         (flatfilter-by-using
          '((always . (("bc" "'bc -lq'")
                       ("df" "'df -h'")
                       ("du" "'du -c -h'")
                       ("du1" "'du --max-depth=1'")
                       ("grep" "'grep --color=auto'")
                       ("ls" "'ls -hF --color=auto'")
                       ("path" "'echo $PATH'")
                       ("ping" "'ping -c 3'")))
            (documents . (("gsmerge" "'gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=temp.pdf'")
                          ("gssplit" "'gs -sDEVICE=pdfwrite -dSAFER -o temp.%d.pdf'")))
            (gentoo . (("esync" "'emerge --sync && notify-send portage-sync-complete'")
                       ("eupg" "'emerge --ask --update --deep --newuse --with-bdeps=y @world'")
                       ("esize" "'qsize -f -m | sort -nk8'")
                       ("esearch" "'emerge --search --fuzzy-search n'")))
            (skydio . (("ac" "'cd ~/aircam'")
                       ("gzl" "'bazel run //tools/gazelle'")
                       ("yubact" "'ssh-add -D && ssh-add -e /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so; ssh-add -s /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so'")))))))
    (string-join (map make-alias aliases) "\n")))

(define (zsh-path)
  (let ((paths
         (flatfilter-by-using
          '((always . ("~/bin" "$path"))
            (skydio . ("${AIRCAM_ROOT}/build/host_aircam/bin"
                       "${AIRCAM_ROOT}/build/host_third_party/bin"))))))
    (string-append "typeset -U path\npath=(" (string-join paths " ") ")")))

(define (dotfile-service service-name file-name file-like)
  (simple-service service-name
                  home-files-service-type
                  (list (list file-name file-like))))

(define (sh-compound statements)
  (string-join statements " && "))

(define (zshrc-files)
  (flatfilter-by-using
   (list (cons 'skydio
               (list
                ;; note: this must happen before autocomplete.zsh
                (plain-file "aircam-completions.zsh"
                            "fpath[1,0]=~/aircam/build/completions/bazel/latest")))
         (cons 'always
               (list
                (plain-file "aliases.zsh" (zsh-aliases))
                (local-file "autocomplete.zsh")
                (local-file "cdhist.zsh")
                (local-file "history.zsh")
                (plain-file "path.zsh" (zsh-path))
                (local-file "prompt.zsh")
                (plain-file "window-title.zsh"
                            (sh-compound
                             '("autoload -Uz add-zsh-hook"
                               "add-zsh-hook precmd window_title")))))
         (cons 'documents (list (local-file "ghostscript.zsh")))
         (cons 'nix
               (list
                (plain-file "nix-profile.zsh"
                            ". ~/.nix-profile/etc/profile.d/nix.sh"))))))

(define (xresources)
  (let ((defines
          ;; solarized colors from http://ethanschoonover.com/solarized
          '(("S_yellow"       "#b58900")
            ("S_orange"       "#cb4b16")
            ("S_red"          "#dc322f")
            ("S_magenta"      "#d33682")
            ("S_violet"       "#6c71c4")
            ("S_blue"         "#268bd2")
            ("S_cyan"         "#2aa198")
            ("S_green"        "#859900")
            ("S_base03"       "#002b36")
            ("S_base02"       "#073642")
            ("S_base01"       "#586e75")
            ("S_base00"       "#657b83")
            ("S_base0"        "#839496")
            ("S_base1"        "#93a1a1")
            ("S_base2"        "#eee8d5")
            ("S_base3"        "#fdf6e3")))
        (settings
         `(("URxvt*background"             "S_base03")
           ("URxvt*foreground"             "S_base0")
           ("URxvt*.depth"                 "32")
           ("URxvt*fading"                 "40")
           ("URxvt*fadeColor"              "S_base03")
           ("URxvt*cursorColor"            "S_base1")
           ("URxvt*pointerColorBackground" "S_base01")
           ("URxvt*pointerColorForeground" "S_base1")
           ("URxvt*color0"                 "S_base02")
           ("URxvt*color1"                 "S_red")
           ("URxvt*color2"                 "S_green")
           ("URxvt*color3"                 "S_yellow")
           ("URxvt*color4"                 "S_blue")
           ("URxvt*color5"                 "S_magenta")
           ("URxvt*color6"                 "S_cyan")
           ("URxvt*color7"                 "S_base2")
           ("URxvt*color8"                 "S_base03")
           ("URxvt*color9"                 "S_orange")
           ("URxvt*color10"                "S_base01")
           ("URxvt*color11"                "S_base00")
           ("URxvt*color12"                "S_base0")
           ("URxvt*color13"                "S_violet")
           ("URxvt*color14"                "S_base1")
           ("URxvt*color15"                "S_base3")
           ("URxvt*.scrollBar"             "false")
           ("URxvt*.font"                  ,(string-append
                                             "xft:Source Code Pro:pixelsize="
                                             (number->string %font-size))))))
    (string-append
     ;; definitions
     (string-join
      (map (lambda (definition)
             (string-append "#define " (car definition) " " (cadr definition)))
           defines)
      "\n")
     "\n"
     ;; config
     (string-join
      (map (lambda (setting)
             (string-append (car setting) ": " (cadr setting)))
           settings)
      "\n")
     "\n")))

(define %use-services
  (filter-by-using
   `((x . ,(dotfile-service 'i3status-dot-script
                            "i3/status.scm"
                            (local-file "i3-status.scm")))
     (x . ,(dotfile-service 'i3status-dot-config
                            "i3/status"
                            (local-file "i3-status")))
     (x . ,(dotfile-service 'rofi-dot-config
                            "config/rofi/config.rasi"
                            (local-file "rofi-config.rasi")))
     (x . ,(dotfile-service 'xresources-dot-config
                            "Xresources"
                            (plain-file "Xresources" (xresources)))))))

(home-environment
  (packages
    (map specification->package %use-packages))
  (services
   (append (list
            (service
             home-zsh-service-type
             (home-zsh-configuration
              (environment-variables %use-env)
              (zshrc (zshrc-files))))
            (dotfile-service 'git-dot-config
                             "gitconfig"
                             (plain-file "gitconfig" (git-config)))
            (dotfile-service 'guix-channels
                            "config/guix/channels.scm"
                            (local-file "guix-channels.scm")))
           %use-services)))

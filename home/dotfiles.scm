(define-module (home dotfiles)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 rdelim))

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
          (make-use 'not-skydio
                    (lambda () (not (is-directory? "/home/skydio"))))
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
                "ghostscript"
                "glibc-locales"
                "htop"
                "julia"
                "ledger"
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
                "yt-dlp"))
     (not-skydio . ("git"            ;; revup uses a custom git
                    "gcc-toolchain"  ;; required by ghc
                    "gdb"))
     (x . ("feh"
           "font-adobe-source-code-pro"
           "fontconfig"
           "mpv"
           "xrandr"
           "xrdb")))))

(define (git-config email)
  (define (config-option option)
    (string-append "\t" (car option) " = " (cadr option)))
  (define (config-section section)
    (let ((header (string-append "[" (car section) "]"))
          (body (map config-option (cadr section))))
      (string-join (cons header body) "\n")))
  (let ((config-tree
         `(("alias"      (("ac"          "log --first-parent")
                          ("locate"     "ls-files --")
                          ("ref"        "rev-parse HEAD")
                          ("st"         "status --short")
                          ("today"      "diff --stat '@{00:00:00}'")
                          ("undo"       "checkout --")
                          ("unstage"    "reset HEAD --")))
           ("color"      (("ui"         "true")))
           ("core"       (("autocrlf"   "input")
                          ("pager"      "less -F -X")))
           ("credential" (("helper"     "cache")))
           ("grep"       (("lineNumber" "true")))
           ("pull"       (("ff"         "only")))
           ("push"       (("default"    "simple")))
           ("rebase"     (("autosquash" "1")))
           ("user"       (("name"       "Brian Kubisiak")
                          ("email"      ,email))))))
    (string-join (map config-section config-tree) "\n")))

(define (git-dotfiles-service email)
  (list (list "gitconfig"
              (plain-file
               "gitconfig" (git-config email)))))

(define-public git-dotfiles-service-type
  (service-type
   (name 'git-dotfiles)
   (extensions
    (list (service-extension home-files-service-type git-dotfiles-service)))
   (description
    "Create a global @file{~/.gitconfig} for the user, given the user's email
address")))

(define (serialize-integer field-name val)
  (serialize-field field-name (number->string val)))

(define-configuration i3-dotfiles-configuration
  (web-browser (package nyxt) "The web browser to use.")
  (font-size (integer 12) "Size of the font to use for chrome."))

(define (i3-config web-browser font-size)
  (format #f "font pango:Source Code Pro ~d
bindsym Mod4+Shift+Return exec urxvt
bindsym Mod4+Shift+w exec ~a
bindsym Mod4+Shift+p exec rofi -show run

bindsym Mod4+f kill
bindsym Mod4+c reload
bindsym Mod4+x exit
bindsym Mod4+e fullscreen

bindsym Mod4+h focus left
bindsym Mod4+j focus down
bindsym Mod4+k focus up
bindsym Mod4+l focus right
bindsym Mod4+y move left
bindsym Mod4+u move down
bindsym Mod4+i move up
bindsym Mod4+o move right

bindsym Mod4+1 workspace number 1
bindsym Mod4+2 workspace number 2
bindsym Mod4+3 workspace number 3
bindsym Mod4+4 workspace number 4
bindsym Mod4+5 workspace number 5
bindsym Mod4+6 workspace number 6
bindsym Mod4+7 workspace number 7
bindsym Mod4+8 workspace number 8
bindsym Mod4+9 workspace number 9
bindsym Mod4+0 workspace number 10
bindsym Mod4+Shift+1 move container to workspace number 1
bindsym Mod4+Shift+2 move container to workspace number 2
bindsym Mod4+Shift+3 move container to workspace number 3
bindsym Mod4+Shift+4 move container to workspace number 4
bindsym Mod4+Shift+5 move container to workspace number 5
bindsym Mod4+Shift+6 move container to workspace number 6
bindsym Mod4+Shift+7 move container to workspace number 7
bindsym Mod4+Shift+8 move container to workspace number 8
bindsym Mod4+Shift+9 move container to workspace number 9
bindsym Mod4+Shift+0 move container to workspace number 10

client.focused          #dc322f #002b36 #839496 #dc322f
client.focused_inactive #dc322f #002b36 #839496 #dc322f
client.unfocused        #002b36 #002b36 #839496 #dc322f
client.urgent           #002b36 #002b36 #839496 #dc322f

bar {
    position bottom
    status_command guile -s ~~/.i3/status.scm
    colors {
        background #002b36
        statusline #839496
        separator  #839496
        focused_workspace  #dc322f #002b36 #839496
        active_workspace   #839496 #002b36 #839496
        inactive_workspace #002b36 #002b36 #586e75
        urgent_workspace   #002b36 #002b36 #586e75
    }
}
" font-size (package-name web-browser)))

(define (i3-dotfiles-service config)
  (let ((web-browser (i3-dotfiles-configuration-web-browser config))
        (font-size (i3-dotfiles-configuration-font-size config)))
    (let ((confile (i3-config web-browser font-size)))
      (list (list "i3/config"
                  (plain-file
                   "i3-config" confile))
            (list "i3/status.scm"
                  (local-file "i3-status.scm"))
            (list "i3/status"
                  (local-file "i3-status"))))))

(define (i3-dotfiles-packages config)
  (list i3-wm
        i3status
        guile-3.0-latest
        (i3-dotfiles-configuration-web-browser config)))

(define-public i3-dotfiles-service-type
  (service-type
   (name 'i3-dotfiles)
   (extensions
    (list (service-extension home-files-service-type
                             i3-dotfiles-service)
          (service-extension home-profile-service-type
                             i3-dotfiles-packages)))
   (default-value (i3-dotfiles-configuration))
   (description
    "Configure the user's i3 installation.")))

(define-configuration urxvt-dotfiles-configuration
  (package (package rxvt-unicode) "Terminal package to install.")
  (font-size (integer 14) "Size of the font to use for the terminal."))

(define (urxvt-dotfiles-packages config)
  (let ((package (urxvt-dotfiles-configuration-package config)))
    (list package)))

(define (urxvt-dotfiles-services config)
  (let ((font-size (urxvt-dotfiles-configuration-font-size config)))
    (list (list "Xresources"
                (plain-file
                 "Xresources" (xresources font-size))))))

(define urxvt-dotfiles-service-type
  (service-type
   (name 'urxvt-dotfiles)
   (extensions
    (list (service-extension home-files-service-type
                             urxvt-dotfiles-services)
          (service-extension home-profile-service-type
                             urxvt-dotfiles-packages)))
   (default-value (urxvt-dotfiles-configuration))
   (description
    "Configure rxvt-unicode via Xresources.")))

(define-configuration rofi-dotfiles-configuration
  (package (package rofi) "Rofi package to install."))

(define (rofi-dotfiles-services config)
  (list (list "config/rofi/config.rasi"
              (local-file "rofi-config.rasi"))))

(define (rofi-dotfiles-packages config)
  (let ((package (rofi-dotfiles-configuration-package config)))
    (list package)))

(define rofi-dotfiles-service-type
  (service-type
   (name 'rofi-dotfiles)
   (extensions
    (list (service-extension home-files-service-type
                             rofi-dotfiles-services)
          (service-extension home-profile-service-type
                             rofi-dotfiles-packages)))
   (default-value (rofi-dotfiles-configuration))
   (description
    "Configure rofi dotfiles.")))

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
                       ("gsmerge" "'gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=temp.pdf'")
                       ("gssplit" "'gs -sDEVICE=pdfwrite -dSAFER -o temp.%d.pdf'")
                       ("ls" "'ls -hF --color=auto'")
                       ("path" "'echo $PATH'")
                       ("ping" "'ping -c 3'")))
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
                (plain-file
                 "nix-profile.zsh"
                 "[ -f ~/.nix-profile/etc/profile.d/nix.sh ] && . ~/.nix-profile/etc/profile.d/nix.sh")
                (plain-file "path.zsh" (zsh-path))
                (local-file "prompt.zsh")
                (plain-file "window-title.zsh"
                            (sh-compound
                             '("autoload -Uz add-zsh-hook"
                               "add-zsh-hook precmd window_title")))
                (local-file "ghostscript.zsh"))))))

(define (xresources font-size)
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
                                             (number->string font-size))))))
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
            (service
             git-dotfiles-service-type
             "brian@kubisiak.com")
            (service
             i3-dotfiles-service-type
             (i3-dotfiles-configuration
              (web-browser nyxt)
              (font-size 10)))
            (service rofi-dotfiles-service-type)
            (service
             urxvt-dotfiles-service-type
             (urxvt-dotfiles-configuration
              (font-size 14)))
            (dotfile-service 'guix-channels
                            "config/guix/channels.scm"
                            (local-file "guix-channels.scm"))))))

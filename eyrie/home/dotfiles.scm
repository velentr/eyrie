;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie home dotfiles)
  #:use-module ((eyrie packages) #:prefix ey:)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages file)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages license)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services configuration)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix transformations)
  #:use-module (ice-9 pretty-print)
  #:export (%core-emacs-plugins
            %core-env
            %core-packages
            %devel-packages
            %nonguix-channel
            %skydio-env
            %skydio-packages
            emacs-dotfiles-service-type
            emacs-dotfiles-configuration
            eyrie-channels-service-type
            git-dotfiles-configuration
            git-dotfiles-service-type
            guile-dotfiles-service-type
            i3-dotfiles-configuration
            i3-dotfiles-service-type
            rofi-dotfiles-service-type
            slideshow-bg-configuration
            slideshow-bg-service-type
            urxvt-dotfiles-configuration
            urxvt-dotfiles-service-type
            zathura-dotfiles-service-type
            ;; TODO: don't export this
            zshrc-files))

(define %core-env
    '(("EDITOR" . "emacs")
      ("HISTFILE" . "${HOME}/.histfile")
      ("HISTSIZE" . "10000")
      ("REPORTTIME" . "5")
      ("SAVEHIST" . "10000")))

(define %skydio-env
  (append
   %core-env
   '(("PYTHONDONTWRITEBYTECODE" . "1")       ;; don't clutter aircam with .pyc
     ("AIRCAM_ROOT" . "/home/skydio/aircam") ;; default to aircam1
     ("AWS_PROFILE" . "default")
     ("CLOUD_CLIENT_EMAIL" . "brian.kubisiak@skydio.com")
     ("SKYCC_LOCAL_JOBS" . "10"))))          ;; more parallelism

(define %core-packages
  (list
   actionlint  ;; for emacs-flycheck on github workflows
   b3sum
   bc
   ey:birdr
   clang-17  ;; for emacs-company autocompletion
   feh
   file
   ey:g-hooks
   ghostscript
   git
   ey:git-third-party
   gitlint
   glibc-locales
   guile-3.0-latest
   guile-colorized
   guile-readline
   htop
   ey:knowledge-store
   le-certs
   ledger
   lsof
   ey:magpie
   ey:magpie-plugins
   man-pages
   man-pages-posix
   mpv
   ncdu
   neomutt
   nmap
   nss-certs
   pass-age
   password-store
   picocom
   psmisc
   python-lsp-server
   reuse
   ey:revup
   ripgrep
   rust
   (list rust "cargo")
   ;; includes rust-analyzer, rustfmt, and clippy
   (list rust "tools")
   setxkbmap
   shellcheck  ;; for emacs-flycheck on shell code
   strace
   tree
   tree-sitter-python
   tree-sitter-rust
   usbutils
   valgrind
   vim
   xrandr
   xrdb
   ey:ytar
   yt-dlp))

(define %skydio-packages
  %core-packages)

(define %devel-packages
  %core-packages)

(define (serialize-integer field-name val)
  (serialize-field field-name (number->string val)))

(define (serialize-string field-name val)
  (serialize-field field-name val))

(define (package-list? val)
  (and (list? val) (and-map package? val)))

(define (serialize-package-list field-name val)
  #f)

(define-configuration git-dotfiles-configuration
  (email (string "") "Email address to use for commit authorship.")
  (github-user (string "") "GitHub username for the gh command-line tool."))

(define (git-config config)
  (define (config-option option)
    (string-append "\t" (car option) " = " (cadr option)))
  (define (config-section section)
    (let ((header (string-append "[" (car section) "]"))
          (body (map config-option (cadr section))))
      (string-join (cons header body) "\n")))
  (let* ((email (git-dotfiles-configuration-email config))
        (github-user (git-dotfiles-configuration-github-user config))
        (core-config-tree
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
           ("trailer \"fix\""
            (("command"   "git log -1 --oneline --format='%h (\\\"%s\\\")' $ARG")
             ("ifExists"  "replace")
             ("ifMissing" "add")
             ("key"       "\"Fixes: \"")
             ("where"     "start")))
           ("user"       (("name"       "Brian Kubisiak")
                          ("email"      ,email)))))
        (config-tree
         (if (equal? github-user "")
             core-config-tree
             (cons `("github" (("user" ,github-user)))
                   core-config-tree))))
    (string-join (map config-section config-tree) "\n")))

(define (git-dotfiles-service config)
  (list (list "git/config"
              (plain-file
               "gitconfig" (git-config config)))))

(define git-dotfiles-service-type
  (service-type
   (name 'git-dotfiles)
   (extensions
    (list (service-extension
           home-xdg-configuration-files-service-type
           git-dotfiles-service)))
   (default-value (git-dotfiles-configuration))
   (description
    "Create a global @file{$XDG_CONFIG_HOME/git/config} for the user, given the
user's email address")))

(define (guile-dotfiles-service sexprs)
  (define (print-contents)
    (for-each
     (lambda (sexpr)
       (pretty-print sexpr))
     sexprs))
  (let ((contents
         (with-output-to-string print-contents)))
    (list (list ".guile"
                  (plain-file "guile" contents)))))

(define guile-dotfiles-service-type
  (service-type
   (name 'guile-dotfiles)
   (extensions
    (list (service-extension home-files-service-type guile-dotfiles-service)))
   (description
    "Create a globel @file{~/.guile} for the user with the given contents")))

(define-configuration i3-dotfiles-configuration
  (web-browser (package nyxt) "The web browser to use.")
  (web-browser-name (string "") "Name of the web browser executable.")
  (font-size (integer 12) "Size of the font to use for chrome.")
  (eth-iface (string "eth1") "Ethernet interface to display the IP address.")
  (status-script (file-like (plain-file "empty" ""))
                 "Scheme script to use for the status."))

(define (i3-config web-browser font-size)
  (format #f "font pango:Source Code Pro ~d
bindsym Mod4+Shift+Return exec urxvt
bindsym Mod4+Shift+w exec ~a
bindsym Mod4+Shift+p exec rofi -show run
bindsym Mod4+Shift+s exec i3lock -c 002b36

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
    status_command guile -e main -s ~~/.i3/run-status
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
" font-size web-browser))

(define (i3-status-config eth-iface)
  (format #f "general {
    output_format = \"none\"
    interval = 1
}
order += \"disk /\"
order += \"ethernet ~a\"
order += \"load\"
order += \"tztime local\"
disk \"/\" {
    format = \"%free/%total\"
}
ethernet \"~a\" {
    format_up = \"E: %ip\"
    format_down = \"E: down\"
}
load {
    format = \"%1min\"
}
tztime local {
    format = \"%d %b, %Y %H:%M\"
}
" eth-iface eth-iface))

(define (i3-dotfiles-service config)
  (let ((web-browser
         (if (equal? (i3-dotfiles-configuration-web-browser-name config) "")
             (package-name (i3-dotfiles-configuration-web-browser config))
             (i3-dotfiles-configuration-web-browser-name config)))
        (font-size (i3-dotfiles-configuration-font-size config))
        (eth-iface (i3-dotfiles-configuration-eth-iface config))
        (status-script (i3-dotfiles-configuration-status-script config)))
    (let ((confile (i3-config web-browser font-size)))
      (list (list ".i3/config"
                  (plain-file
                   "i3-config" confile))
            (list ".i3/status"
                  (plain-file
                   "i3-status-config" (i3-status-config eth-iface)))
            (list ".i3/run-status" status-script)))))

(define (i3-dotfiles-packages config)
  (list i3-wm
        i3status
        font-adobe-source-code-pro
        fontconfig
        guile-3.0-latest
        (i3-dotfiles-configuration-web-browser config)))

(define i3-dotfiles-service-type
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

(define (eyrie-channels-service channel-list)
  channel-list)

(define %nonguix-channel
  (channel
   (name 'nonguix)
   (url "https://gitlab.com/nonguix/nonguix")
   (introduction
    (make-channel-introduction
     "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
     (openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29 12AF 6F51 20A0 22FB B2D5")))))

(define eyrie-channels-service-type
  (service-type
   (name 'eyrie-channels)
   (extensions
    (list (service-extension home-channels-service-type
                             eyrie-channels-service)))
   (default-value
     (list %nonguix-channel))
   (description
    "Add additional guix channels.")))

(define (urxvt-dotfiles-packages config)
  (let ((package (urxvt-dotfiles-configuration-package config)))
    (list package
          font-adobe-source-code-pro
          fontconfig)))

(define (urxvt-dotfiles-services config)
  (let ((font-size (urxvt-dotfiles-configuration-font-size config)))
    (list (list ".Xresources"
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
  (list (list "rofi/config.rasi"
              (local-file "rofi-config.rasi"))))

(define (rofi-dotfiles-packages config)
  (let ((package (rofi-dotfiles-configuration-package config)))
    (list package
          font-adobe-source-code-pro
          fontconfig)))

(define rofi-dotfiles-service-type
  (service-type
   (name 'rofi-dotfiles)
   (extensions
    (list (service-extension home-xdg-configuration-files-service-type
                             rofi-dotfiles-services)
          (service-extension home-profile-service-type
                             rofi-dotfiles-packages)))
   (default-value (rofi-dotfiles-configuration))
   (description
    "Configure rofi dotfiles.")))

(define-configuration zathura-dotfiles-configuration
  (package (package zathura) "Zathura package to install")
  (plugins (package-list (list zathura-pdf-poppler)) "File plugins to install"))

(define (zathura-dotfiles-services config)
  (list (list "zathura/zathurarc"
              (local-file "zathurarc"))))

(define (zathura-dotfiles-packages config)
  (let ((package (zathura-dotfiles-configuration-package config))
        (plugins (zathura-dotfiles-configuration-plugins config)))
    (cons package plugins)))

(define zathura-dotfiles-service-type
  (service-type
   (name 'zathura-dotfiles)
   (extensions
    (list (service-extension home-xdg-configuration-files-service-type
                             zathura-dotfiles-services)
          (service-extension home-profile-service-type
                             zathura-dotfiles-packages)))
   (default-value (zathura-dotfiles-configuration))
   (description "Configure zathura and install plugins.")))

(define (zsh-aliases skydio?)
  (define (make-alias alias)
    (let ((name (car alias))
          (value (cadr alias)))
      (string-append "alias " name "=" value)))
  (let ((aliases
         '(("bc" "'bc -lq'")
           ("df" "'df -h'")
           ("du" "'du -c -h'")
           ("du1" "'du --max-depth=1'")
           ("grep" "'grep --color=auto'")
           ("gsmerge" "'gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=temp.pdf'")
           ("gssplit" "'gs -sDEVICE=pdfwrite -dSAFER -o temp.%d.pdf'")
           ("ls" "'ls -hF --color=auto'")
           ("path" "'echo $PATH'")
           ("ping" "'ping -c 3'")))
        (skydio-aliases
         '(("ac" "'cd ~/aircam'")
           ("acfmt" "'bazel run //tools/code_linter:code_format --'")
           ("awsauth" "'bazel -S run //auth/aws'")
           ("cloudstation" "'bazel run //cloud/dev_instances --'")
           ("ffp" "'bazel run //infrastructure/tools/flashpack:fetch_flashpack_ng -- --email brian.kubisiak@skydio.com'")
           ("gzl" "'bazel run //tools/gazelle'")
           ("rs-an-up" "'bazel -S run @rules_rust//tools/rust_analyzer:gen_rust_project'")
           ("rs-cr-up" "'bazel -S run //infrastructure/rust:crates_vendor -- --repin workspace'")
           ("yubact" "'ssh-add -D && ssh-add -e /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so; ssh-add -s /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so'"))))
    (string-join (map make-alias
                      (if skydio?
                          (append aliases skydio-aliases)
                          aliases))
                 "\n")))

(define (zsh-path skydio?)
  (let ((core-paths
         '("~/bin" "$path"))
        (skydio-paths
         '("${AIRCAM_ROOT}/build/host_aircam/bin"
           "${AIRCAM_ROOT}/build/host_third_party/bin")))
    (string-append
     "typeset -U path\npath=("
     (string-join
      (if skydio?
          (append core-paths skydio-paths)
          core-paths)
      " ")
     ")")))

(define (sh-compound statements)
  (string-join statements " && "))

(define (zshrc-files skydio?)
  (let ((core-files
         (list
          (plain-file "aliases.zsh" (zsh-aliases skydio?))
          (local-file "autocomplete.zsh")
          (local-file "cdhist.zsh")
          (local-file "history.zsh")
          (plain-file
           "nix-profile.zsh"
           "[ -f ~/.nix-profile/etc/profile.d/nix.sh ] && . ~/.nix-profile/etc/profile.d/nix.sh")
          (plain-file "path.zsh" (zsh-path skydio?))
          (local-file "prompt.zsh")
          (local-file "utils.zsh")
          (plain-file "window-title.zsh"
                      (sh-compound
                       '("autoload -Uz add-zsh-hook"
                         "add-zsh-hook precmd window_title")))
          (local-file "ghostscript.zsh"))))
    (if skydio?
        (cons
         ;; note: this must happen before autocomplete.zsh
         (plain-file "aircam-completions.zsh"
                     "fpath[1,0]=~/aircam/build/completions/bazel/latest")
         core-files)
        core-files)))

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
           ("URxvt*.saveLines"             "8192")
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

(define %core-emacs-plugins
  (list emacs-bazel
        emacs-color-theme-solarized
        ey:emacs-combobulate
        emacs-company
        emacs-dts-mode
        emacs-evil
        emacs-evil-org
        ey:emacs-evil-textobj-tree-sitter
        emacs-flycheck
        ey:emacs-github-mode
        emacs-go-mode
        emacs-markdown-mode
        emacs-nix-mode
        emacs-paredit
        emacs-org
        emacs-rust-mode
        emacs-systemd-mode
        emacs-terraform-mode
        emacs-yaml-mode))

(define-configuration emacs-dotfiles-configuration
  (package (package emacs-no-x) "Emacs package to install")
  (plugins (package-list %core-emacs-plugins)
           "Emacs plugins to install"))

(define (emacs-dotfiles-services config)
  (list (list ".emacs"
              (local-file "emacsrc.el"))))

(define (emacs-dotfiles-packages config)
  (let ((package (emacs-dotfiles-configuration-package config))
        (plugins (emacs-dotfiles-configuration-plugins config)))
    (cons package plugins)))

(define emacs-dotfiles-service-type
  (service-type
   (name 'emacs-dotfiles)
   (extensions
    (list (service-extension home-files-service-type
                             emacs-dotfiles-services)
          (service-extension home-profile-service-type
                             emacs-dotfiles-packages)))
   (default-value (emacs-dotfiles-configuration))
   (description "Configure emacs and install plugins.")))

(define (list-or-string? val)
  (or (list? val)
      (string? val)))

(define-configuration/no-serialization slideshow-bg-configuration
  (directory (string (string-append (getenv "HOME") "/solarized"))
             "Directory containing images to select from.")
  (interval (list-or-string ''(next-hour '(0))) ; daily at midnight
            "GitHub username for the gh command-line tool."))

(define (slideshow-bg-mcron-jobs config)
  (match-record config <slideshow-bg-configuration>
                (directory interval)
    (list #~(job #$interval
                 (string-append
                  #$feh
                  "/bin/feh --bg-fill $("
                  #$findutils
                  "/bin/find "
                  #$directory
                  " -type f | "
                  #$coreutils
                  "/bin/shuf -n 1)")))))

(define slideshow-bg-service-type
  (service-type
   (name 'slideshow-bg)
   (extensions
    (list (service-extension home-mcron-service-type
                             slideshow-bg-mcron-jobs)))
   (default-value (slideshow-bg-configuration))
   (description "Periodically change the desktop background.")))

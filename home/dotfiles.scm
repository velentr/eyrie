;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (dotfiles)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages julia)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 pretty-print)
  #:export (%cad-packages
            %core-env
            %devel-packages
            %skydio-env
            %skydio-packages
            git-dotfiles-service-type
            guix-channels-service-type
            i3-dotfiles-configuration
            i3-dotfiles-service-type
            rofi-dotfiles-service-type
            urxvt-dotfiles-configuration
            urxvt-dotfiles-service-type
            zathura-dotfiles-service-type
            ;; TODO: don't export this
            zshrc-files))

(define %core-env
    '(("EDITOR" . "emacs")
      ("HISTFILE" . "~/.histfile")
      ("HISTSIZE" . "10000")
      ("REPORTTIME" . "5")
      ("SAVEHIST" . "10000")))

(define %skydio-env
  (append
   %core-env
   '(("PYTHONDONTWRITEBYTECODE" . "1")       ;; don't clutter aircam with .pyc
     ("AIRCAM_ROOT" . "/home/skydio/aircam") ;; default to aircam1
     ("AWS_PROFILE" . "default")
     ("SKYCC_LOCAL_JOBS" . "10"))))          ;; more parallelism

(define %core-packages
  (map specification->package
       '("bc"
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
         "feh"
         "file"
         "ghostscript"
         "glibc-locales"
         "htop"
         "le-certs"
         "ledger"
         "lsof"
         "mpv"
         "ncdu"
         "neomutt"
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
         "xrandr"
         "xrdb"
         "yt-dlp")))

(define %skydio-packages
  %core-packages)

(define %devel-packages
  (append (list
           erlang
           ghc
           ghc-hunit
           git  ;; revup uses a custom git
           gcc  ;; required by ghc
           gdb
           julia
           lua
           pinentry-tty
           rust
           (list rust "cargo")
           (list rust "rustfmt"))
          %core-packages))

(define %cad-packages
  (list
   emacs-scad-mode
   openscad
   prusa-slicer))

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

(define git-dotfiles-service-type
  (service-type
   (name 'git-dotfiles)
   (extensions
    (list (service-extension home-files-service-type git-dotfiles-service)))
   (description
    "Create a global @file{~/.gitconfig} for the user, given the user's email
address")))

(define (serialize-integer field-name val)
  (serialize-field field-name (number->string val)))

(define (serialize-string field-name val)
  (serialize-field field-name val))

(define (package-list? val)
  (and (list? val) (and-map package? val)))

(define (serialize-package-list field-name val)
  #f)

(define-configuration i3-dotfiles-configuration
  (web-browser (package nyxt) "The web browser to use.")
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
  (let ((web-browser (i3-dotfiles-configuration-web-browser config))
        (font-size (i3-dotfiles-configuration-font-size config))
        (eth-iface (i3-dotfiles-configuration-eth-iface config))
        (status-script (i3-dotfiles-configuration-status-script config)))
    (let ((confile (i3-config web-browser font-size)))
      (list (list "i3/config"
                  (plain-file
                   "i3-config" confile))
            (list "i3/status"
                  (plain-file
                   "i3-status-config" (i3-status-config eth-iface)))
            (list "i3/status.scm" status-script)))))

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

(define (guix-channels-services channel-list)
  (define (print-contents)
    (pretty-print `(append ,channel-list %default-channels)))
  (let ((contents
         (with-output-to-string print-contents)))
    (list (list "config/guix/channels.scm"
                  (plain-file
                   "guix-channels" contents)))))


(define guix-channels-service-type
  (service-type
   (name 'guix-channels)
   (extensions
    (list (service-extension home-files-service-type
                             guix-channels-services)))
   (default-value '())
   (description
    "Add additional guix channels via ~/.config/guix-channels.scm.")))

(define (urxvt-dotfiles-packages config)
  (let ((package (urxvt-dotfiles-configuration-package config)))
    (list package
          font-adobe-source-code-pro
          fontconfig)))

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
    (list package
          font-adobe-source-code-pro
          fontconfig)))

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

(define-configuration zathura-dotfiles-configuration
  (package (package zathura) "Zathura package to install")
  (plugins (package-list (list zathura-pdf-poppler)) "File plugins to install"))

(define (zathura-dotfiles-services config)
  (list (list "config/zathura/zathurarc"
              (local-file "zathurarc"))))

(define (zathura-dotfiles-packages config)
  (let ((package (zathura-dotfiles-configuration-package config))
        (plugins (zathura-dotfiles-configuration-plugins config)))
    (cons package plugins)))

(define zathura-dotfiles-service-type
  (service-type
   (name 'zathura-dotfiles)
   (extensions
    (list (service-extension home-files-service-type
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
           ("gzl" "'bazel run //tools/gazelle'")
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

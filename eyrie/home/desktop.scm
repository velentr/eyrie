;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie home desktop)
  #:use-module (eyrie home dotfiles)
  #:use-module (eyrie home rsnapshot)
  #:use-module (gnu home)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages librewolf)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(home-environment
  (packages
   (cons* emacs-scad-mode pinentry-tty %core-packages))
  (services
   (append (list
            (service emacs-dotfiles-service-type)
            (service
             home-openssh-service-type
             (home-openssh-configuration
              (hosts
               (list (openssh-host (name "e3r3.com")
                                   (user "root"))
                     (openssh-host (name "rsync.net")
                                   (host-name "fm1436.rsync.net")
                                   (user "fm1436"))))))
            (service
             home-rsnapshot-service-type
             (home-rsnapshot-configuration
              (backups
               `(("root@e3r3.com:/var/lib/radicale/" "e3r3")
                 (,(string-append (getenv "XDG_DATA_HOME") "/birdr/birds.db")
                  "peregrine")
                 (,(string-append (getenv "HOME") "/.ledger") "peregrine")))))
            (service
             home-zsh-service-type
             (home-zsh-configuration
              (environment-variables %core-env)
              (zshrc (zshrc-files #f))))
            (service
             git-dotfiles-service-type
             (git-dotfiles-configuration
              (email "brian@kubisiak.com")))
            (service
             guile-dotfiles-service-type
             '((use-modules (ice-9 readline))
               (activate-readline)
               (unless (getenv "INSIDE_EMACS")
                 (use-modules (ice-9 colorized))
                 (activate-colorized))))
            (service eyrie-channels-service-type)
            (service home-ssh-agent-service-type)
            (service
             i3-dotfiles-service-type
             (i3-dotfiles-configuration
              (web-browser librewolf)
              (web-browser-name "librewolf --private-window")
              (font-size 10)
              (eth-iface "enp4s0")
              (status-script (local-file "run-status"))))
            (service slideshow-bg-service-type)
            (service rofi-dotfiles-service-type)
            (service
             urxvt-dotfiles-service-type
             (urxvt-dotfiles-configuration
              (font-size 14)))
            (service zathura-dotfiles-service-type)))))

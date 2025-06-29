;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie home laptop)
  #:use-module (eyrie home dotfiles)
  #:use-module (gnu home)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu packages librewolf)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(home-environment
 (packages
  (cons*
   acpi
   openssh-sans-x ; for ssh client
   %core-packages))
 (services
  (append (list
           (service emacs-dotfiles-service-type)
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
            home-openssh-service-type
            (home-openssh-configuration
             (hosts
              (list
               (openssh-host (name "peregrine")
                             (host-name "10.10.0.5")
                             (user "bkubisiak")
                             (port 2222))
               (openssh-host (name "e3r3.com")
                             (user "root"))
               (openssh-host (name "rsync.net")
                             (host-name "fm1436.rsync.net")
                             (user "fm1436"))))))
           (service
            i3-dotfiles-service-type
            (i3-dotfiles-configuration
             (web-browser librewolf)
             (web-browser-name "librewolf --private-window")
             (font-size 10)
             (eth-iface "wlp58s0")
             (status-script (local-file "run-status"))))
           (service rofi-dotfiles-service-type)
           (service
            urxvt-dotfiles-service-type
            (urxvt-dotfiles-configuration
             (font-size 14)))
           (service zathura-dotfiles-service-type)))))

;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie home laptop)
  #:use-module (eyrie home dotfiles)
  #:use-module (gnu home)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu packages browser-extensions)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(home-environment
 (packages
  (cons* acpi ublock-origin/chromium %core-packages))
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
            i3-dotfiles-service-type
            (i3-dotfiles-configuration
             (web-browser ungoogled-chromium)
             (web-browser-name "chromium")
             (font-size 10)
             (eth-iface "wlp58s0")
             (status-script (local-file "run-status"))))
           (service rofi-dotfiles-service-type)
           (service
            urxvt-dotfiles-service-type
            (urxvt-dotfiles-configuration
             (font-size 14)))
           (service zathura-dotfiles-service-type)))))

;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie home skydio-desktop)
  #:use-module (eyrie home dotfiles)
  #:use-module (gnu home)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(home-environment
  (packages %skydio-packages)
  (services
   (append (list
            (service
             emacs-dotfiles-service-type
             (emacs-dotfiles-configuration
              (plugins (cons* emacs-dockerfile-mode
                              emacs-protobuf-mode
                              %core-emacs-plugins))))
            (service
             home-mcron-service-type
             (home-mcron-configuration
              (jobs (list #~(job '(next-hour '(0))
                                 #$(file-append
                                    nix
                                    "/bin/nix store optimise"))))))
            (service
             home-zsh-service-type
             (home-zsh-configuration
              (environment-variables %skydio-env)
              (zshrc (zshrc-files #t))))
            (service
             git-dotfiles-service-type
             (git-dotfiles-configuration
              (email "brian.kubisiak@skydio.com")
              (github-user "brian-kubisiak-skydio")))
            (service
             guile-dotfiles-service-type
             '((use-modules (ice-9 readline))
               (activate-readline)
               (unless (getenv "INSIDE_EMACS")
                 (use-modules (ice-9 colorized))
                 (activate-colorized))))
            (service eyrie-channels-service-type)
            (service
             i3-dotfiles-service-type
             (i3-dotfiles-configuration
              (web-browser ungoogled-chromium)
              (web-browser-name "chromium")
              (font-size 10)
              (eth-iface "ens6f0")
              (status-script (local-file "run-status"))))
            (service rofi-dotfiles-service-type)
            (service
             urxvt-dotfiles-service-type
             (urxvt-dotfiles-configuration
              (font-size 12)))
            (service zathura-dotfiles-service-type)))))

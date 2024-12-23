;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie home skydio-laptop)
  #:use-module (eyrie home dotfiles)
  #:use-module (gnu home)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages chrome))

(home-environment
  (packages %skydio-packages)
  (services
   (append (list
            (service
             home-zsh-service-type
             (home-zsh-configuration
              (environment-variables %skydio-env)
              (zshrc (zshrc-files #t))))
            (service emacs-dotfiles-service-type)
            (service
             git-dotfiles-service-type
             (git-dotfiles-configuration
              (email "brian.kubisiak@skydio.com")
              (github-user "brian-kubisiak-skydio")))
            (service eyrie-channels-service-type)
            (service
             i3-dotfiles-service-type
             (i3-dotfiles-configuration
              (web-browser google-chrome-stable)
              (web-browser-name "google-chrome")
              (font-size 10)
              (eth-iface "wlan0")
              (status-script (local-file "run-status"))))
            (service rofi-dotfiles-service-type)
            (service
             urxvt-dotfiles-service-type
             (urxvt-dotfiles-configuration
              (font-size 15)))
            (service zathura-dotfiles-service-type)))))

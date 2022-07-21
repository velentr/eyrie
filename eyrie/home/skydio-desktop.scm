;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie home skydio-desktop)
  #:use-module (eyrie home dotfiles)
  #:use-module (gnu home)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(home-environment
  (packages %skydio-packages)
  (services
   (append (list
            (service
             emacs-dotfiles-service-type
             (emacs-dotfiles-configuration
              (plugins (cons emacs-org-jira %core-emacs-plugins))))
            (service
             home-zsh-service-type
             (home-zsh-configuration
              (environment-variables %skydio-env)
              (zshrc (zshrc-files #t))))
            (service
             git-dotfiles-service-type
             "brian.kubisiak@skydio.com")
            (service
             guix-channels-service-type
             '(list
               (channel (name 'nonguix)
                        (url "https://gitlab.com/nonguix/nonguix")
                        (introduction
                         (make-channel-introduction
                          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                          (openpgp-fingerprint
                           "2A39 3FFF 68F4 EF7A 3D29 12AF 6F51 20A0 22FB B2D5"))))))
            (service
             i3-dotfiles-service-type
             (i3-dotfiles-configuration
              (web-browser ungoogled-chromium)
              (web-browser-name "chromium")
              (font-size 10)
              (eth-iface "eth0")
              (status-script (local-file "run-status"))))
            (service rofi-dotfiles-service-type)
            (service
             urxvt-dotfiles-service-type
             (urxvt-dotfiles-configuration
              (font-size 12)))
            (service zathura-dotfiles-service-type)))))

;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie home laptop)
  #:use-module (eyrie home dotfiles)
  #:use-module (gnu home)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(home-environment
 (packages
  (cons acpi %core-packages))
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
            "brian@kubisiak.com")
           (service
            guile-dotfiles-service-type
            '((use-modules (ice-9 readline))
              (activate-readline)
              (unless (getenv "INSIDE_EMACS")
                (use-modules (ice-9 colorized))
                (activate-colorized))))
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

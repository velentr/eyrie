;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(use-modules (dotfiles)
             (gnu home)
             (gnu home services shells)
             (gnu packages web-browsers)
             (gnu services)
             (guix gexp))

(home-environment
  (packages (append %devel-packages %cad-packages))
  (services
   (append (list
            (service
             home-zsh-service-type
             (home-zsh-configuration
              (environment-variables %core-env)
              (zshrc (zshrc-files #f))))
            (service
             git-dotfiles-service-type
             "brian@kubisiak.com")
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
              (web-browser nyxt)
              (font-size 10)
              (eth-iface "enp4s0")
              (status-script (local-file "i3-status.scm"))))
            (service rofi-dotfiles-service-type)
            (service
             urxvt-dotfiles-service-type
             (urxvt-dotfiles-configuration
              (font-size 14)))
            (service zathura-dotfiles-service-type)))))

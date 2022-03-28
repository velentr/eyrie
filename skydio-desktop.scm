;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(use-modules (dotfiles)
             (gnu home)
             (gnu home services shells)
             (gnu packages chromium)
             (gnu services)
             (guix gexp))

(home-environment
  (packages %skydio-packages)
  (services
   (append (list
            (service
             home-zsh-service-type
             (home-zsh-configuration
              (environment-variables %skydio-env)
              (zshrc (zshrc-files #t))))
            (service
             git-dotfiles-service-type
             "brian.kubisiak@skydio.com")
            (service
             i3-dotfiles-service-type
             (i3-dotfiles-configuration
              (web-browser ungoogled-chromium)
              (web-browser-name "chromium")
              (font-size 10)
              (eth-iface "eth0")
              (status-script (local-file "i3-status.scm"))))
            (service rofi-dotfiles-service-type)
            (service
             urxvt-dotfiles-service-type
             (urxvt-dotfiles-configuration
              (font-size 12)))
            (service zathura-dotfiles-service-type)))))

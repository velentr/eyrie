;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(use-modules (gnu)
             (nongnu packages linux)
             (nongnu system linux-initrd))
(use-service-modules desktop networking nix ssh xorg)
(use-package-modules certs package-management shells ssh)

(define %encrypted-root
  (mapped-device
   ;; found via: cryptsetup luksUUID /dev/sdb3
   (source (uuid "65d8fcab-f426-43c4-951d-46ac46a0561e"))
   (target "root")
   (type luks-device-mapping)))

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (host-name "peregrine")
 (timezone "US/Pacific")
 (locale "en_US.utf8")

 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets '("/boot/efi"))))
 (mapped-devices (list %encrypted-root))
 (file-systems (append
                (list (file-system
                       (device "/dev/mapper/root")
                       (mount-point "/")
                       (type "btrfs")
                       (dependencies (list %encrypted-root)))
                      (file-system
                       (device (file-system-label "BOOTEFI"))
                       (mount-point "/boot/efi")
                       (type "vfat")))
                %base-file-systems))

 (users (cons (user-account
               (name "bkubisiak")
               (group "users")
               (shell (file-append zsh "/bin/zsh"))
               (supplementary-groups '("wheel"
                                       "audio" "video")))
              %base-user-accounts))

 (packages (append (list nss-certs) %base-packages))

 (services (append (list (elogind-service)
                         (service static-networking-service-type
                                  (list (static-networking
                                         (addresses
                                          (list (network-address
                                                 (device "enp4s0")
                                                 (value "10.10.0.5/16"))))
                                         (routes
                                          (list (network-route
                                                 (destination "default")
                                                 (gateway "10.10.0.1"))))
                                         (name-servers '("208.67.220.220"
                                                         "208.67.220.222"
                                                         "208.67.222.220"
                                                         "208.67.222.222")))))
                         (service ntp-service-type)
                         (service openssh-service-type
                                  (openssh-configuration
                                   (openssh openssh-sans-x)
                                   (password-authentication? #f)
                                   (port-number 2222)))
                         (service slim-service-type
                                  (slim-configuration
                                   (display ":0"))))
                   %base-services)))

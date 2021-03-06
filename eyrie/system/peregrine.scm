;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie system peregrine)
  #:use-module (gnu)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages wm)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services security-token)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages printers)
  #:use-module (nongnu system linux-initrd))

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
               (supplementary-groups '("wheel" "audio" "video" "lp" "lpadmin")))
              %base-user-accounts))

 (packages (append (list le-certs nss-certs) %base-packages))

 (services (append (list (elogind-service)
                         (screen-locker-service i3lock)
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
                         (service pcscd-service-type)
                         (service slim-service-type
                                  (slim-configuration
                                   (display ":0")))
                         (service cups-service-type
                                  (cups-configuration
                                   (web-interface? #t)
                                   (extensions
                                    (list cups-filters hplip)))))
                   %base-services)))

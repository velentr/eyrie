;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(use-modules (gnu))
(use-service-modules desktop networking nix ssh)
(use-package-modules certs package-management shells ssh)

(define %encrypted-root
  (mapped-device
   ;; found via: cryptsetup luksUUID /dev/sdb3
   (source (uuid "fb340d7c-fa8e-49ac-890a-50c95e90b950"))
   (target "root")
   (type luks-device-mapping)))

(define %encrypted-share
  (mapped-device
   (source (uuid "97a4f05b-4e50-4cd4-9a90-3505f2b4201c"))
   (target "share")
   (type luks-device-mapping)))

(operating-system
 (host-name "redtail")
 (timezone "US/Pacific")
 (locale "en_US.utf8")

 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets '("/boot/efi"))))
 ;; hid_microsoft for using my ms ergo keyboard to enter the encryption key
 (initrd-modules (cons "hid_microsoft" %base-initrd-modules))
 (mapped-devices (list %encrypted-root %encrypted-share))
 (file-systems (append
                (list (file-system
                       (device "/dev/mapper/root")
                       (mount-point "/")
                       (type "btrfs")
                       (dependencies (list %encrypted-root)))
                      (file-system
                       (device (file-system-label "BOOTEFI"))
                       (mount-point "/boot/efi")
                       (type "vfat"))
                      (file-system
                       (device "/dev/mapper/share")
                       (mount-point "/share")
                       (type "ext4")
                       (dependencies (list %encrypted-share))))
                %base-file-systems))

 (users (cons (user-account
               (name "bkubisiak")
               (group "users")
               (shell (file-append zsh "/bin/zsh"))
               (supplementary-groups '("wheel"
                                       "audio" "video")))
              %base-user-accounts))

 (packages (append (list nix nss-certs) %base-packages))

 (services (append (list (elogind-service)  ;; to create /run/user/${UID} on login
                         (static-networking-service
                          "enp2s0" "10.10.0.4"
                          #:netmask "255.255.0.0"
                          #:gateway "10.10.0.1"
                          #:name-servers '("208.67.220.220"
                                           "208.67.220.222"
                                           "208.67.222.220"
                                           "208.67.222.222"))
                         (service nix-service-type)
                         (service ntp-service-type)
                         (service openssh-service-type
                                  (openssh-configuration
                                   (openssh openssh-sans-x)
                                   (password-authentication? #f)
                                   (port-number 2222))))
                   %base-services)))

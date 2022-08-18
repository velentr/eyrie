;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie system kestrel)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages wm)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(define %encrypted-root
  (mapped-device
   ;; found via: cryptsetup luksUUID /dev/sdb3
   (source (uuid "cffce105-7053-43cc-b352-73a68ad43a8e"))
   (target "root")
   (type luks-device-mapping)))

(operating-system
 (host-name "kestrel")
 (timezone "US/Pacific")
 (locale "en_US.utf8")
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list atheros-firmware linux-firmware))

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
	       (supplementary-groups '("audio" "netdev" "video" "wheel")))
              %base-user-accounts))

 (packages (append (list le-certs nss-certs wpa-supplicant) %base-packages))

 (services (append (list (elogind-service)  ;; to create /run/user/${UID} on login
                         (screen-locker-service i3lock)
			 (service network-manager-service-type)
                         (service ntp-service-type)
                         (service slim-service-type
                                  (slim-configuration
                                   (display ":0")))
                         (service openssh-service-type
                                  (openssh-configuration
                                   (openssh openssh-sans-x)
                                   (password-authentication? #f)
                                   (port-number 2222)))
                         (service wpa-supplicant-service-type))
                   %base-services)))

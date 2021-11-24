(use-modules (gnu))
(use-service-modules networking ssh)
(use-package-modules certs shells ssh)

(define %encrypted-root
  (mapped-device
   ;; found via: cryptsetup luksUUID /dev/sdb3
   (source (uuid "fb340d7c-fa8e-49ac-890a-50c95e90b950"))
   (target "root")
   (type luks-device-mapping)))

(operating-system
 (host-name "redtail")
 (timezone "US/Pacific")
 (locale "en_US.utf8")

 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (target "/boot/efi")))
 ;; hid_microsoft for using my ms ergo keyboard to enter the encryption key
 (initrd-modules (cons "hid_microsoft" %base-initrd-modules))
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

 (packages (cons nss-certs %base-packages))

 (services (append (list (static-networking-service
                          "enp2s0" "10.10.0.4"
                          #:netmask "255.255.0.0"
                          #:gateway "10.10.0.1"
                          #:name-servers '("208.67.220.220"
                                           "208.67.220.222"
                                           "208.67.222.220"
                                           "208.67.222.222"))
                         (service openssh-service-type
                                  (openssh-configuration
                                   (openssh openssh-sans-x)
                                   (port-number 2222))))
                   %base-services)))

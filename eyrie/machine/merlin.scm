;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie machine merlin)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (gnu system locale)
  #:use-module (guix platforms arm)
  #:use-module (micrognu packages updates)
  #:use-module (micrognu packages vendor globalscaletechnologies)
  #:use-module (micrognu partition)
  #:use-module (micrognu services bootloader)
  #:use-module (eyrie system keys))

(define espressobin-ultra-barebones-os
  (operating-system
    (kernel linux-espressobin-ultra-5.4)
    (kernel-arguments
     (cons*
      ;; note that we actually want driver_mode=2, but that will still bring up
      ;; sta since the driver always enables bit 0
      "mwifiex.driver_mode=3"
      ;; net.ifnames=1 will randomly rename one of uap0 and mlan0; ironically
      ;; disabling it makes the naming more consistent.
      "net.ifnames=0"
      "console=ttyMV0,115200"
      %default-kernel-arguments))
    (initrd-modules '())
    (firmware (list espressobin-ultra-linux-firmware))
    (host-name "ebinx")
    (timezone "US/Pacific")
    (locale "en_US.utf8")
    (locale-definitions (list (locale-definition
                               (name "en_US.utf8")
                               (source "en_US")
                               (charset "UTF-8"))))
    (locale-libcs (list glibc))
    (bootloader (bootloader-configuration
                 (bootloader u-boot-bootloader)
                 (targets '("/dev/mmcblk0"))))
    (file-systems (cons*
                   (file-system
                     (device "/dev/mmcblk0p1")
                     (mount-point "/")
                     (type "ext4"))
                   (file-system
                     (device "/dev/mmcblk0p3")
                     (mount-point "/data")
                     (type "ext4"))
                   %base-file-systems))))

(define espressobin-ultra-image-type
  (image-type
   (name 'espressobin-ultra-raw)
   (constructor (lambda (os)
                  (image
                   (inherit (raw-with-offset-disk-image))
                   (operating-system os)
                   (platform aarch64-linux))))))

(define espressobin-ultra-barebones-raw-image
  (image
   (inherit
    (os+platform->image espressobin-ultra-barebones-os aarch64-linux
                        #:type espressobin-ultra-image-type))
   (name 'espressobin-ultra-raw-image)))

(define merlin-operating-system
  (operating-system
    (inherit espressobin-ultra-barebones-os)
    (packages
     (cons* rauc
            u-boot-tools
            (append
             %base-packages-linux
             %base-packages-networking
             %base-packages-utils)))
    (setuid-programs '())
    (host-name "merlin")
    (services
     (cons* (service static-networking-service-type
                     (list (static-networking
                            (links
                             (list
                              (network-link
                               (name "br0")
                               (type 'bridge)
                               (arguments '()))
                              (network-link
                               (name "lan0")
                               (arguments '((master . "br0"))))
                              ;; this usually runs before uap0 comes up and this
                              ;; step fails, but the hostapd will add the bridge
                              ;; from the bridge=br0 config option
                              (network-link
                               (name "uap0")
                               (arguments '((master . "br0"))))))
                            (addresses
                             (list
                              ;; note that eth0 must be up before attempting to
                              ;; bring up the lan* interfaces; use 0.0.0.0 since
                              ;; it doesn't actually have an address
                              (network-address
                               (device "eth0")
                               (value "0.0.0.0/0"))
                              (network-address
                               (device "br0")
                               (value "10.10.0.6/16"))
                              (network-address
                               (device "lan0")
                               (value "0.0.0.0/0"))))
                            (routes
                             (list (network-route
                                    (destination "default")
                                    (gateway "10.10.0.1"))))
                            (name-servers '("208.67.220.220"
                                            "208.67.220.222"
                                            "208.67.222.220"
                                            "208.67.222.222")))))
            (service openssh-service-type
                     (openssh-configuration
                      (openssh openssh-sans-x)
                      (permit-root-login 'prohibit-password)
                      (password-authentication? #f)
                      (authorized-keys
                       (list (list "root"
                                   %condor-ssh-key
                                   %peregrine-ssh-key)))))
            (service hostapd-service-type
                     (hostapd-configuration
                      (interface "uap0")
                      (ssid "sky.net")
                      (channel 165)
                      (extra-settings "
hw_mode=a
bridge=br0
ieee80211d=1
country_code=US
ieee80211n=1
ieee80211ac=1
auth_algs=1
wpa=2
wpa_key_mgmt=WPA-PSK
wpa_psk_file=/data/hostapd-wpa-psk.txt
wpa_pairwise=CCMP
")))
            (service u-boot-env-service-type
                     (list
                      (u-boot-env-configuration
                       (device "/dev/mtdblock2")
                       (size #x10000)
                       (sector-size #x1000)
                       (number-of-sectors 16))))
            (modify-services %base-services
              (delete guix-service-type))))))

(define merlin-partition
  (rootfs-partition
   #:partition
   (partition
    (inherit root-partition)
    (uuid (operating-system-uuid merlin-operating-system))
    (label "merlin-root"))
   #:os merlin-operating-system
   #:target "aarch64-linux-gnu"))

merlin-partition

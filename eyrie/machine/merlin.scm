;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie machine merlin)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix platforms arm)
  #:use-module (micrognu packages updates)
  #:use-module (micrognu packages vendor globalscaletechnologies)
  #:use-module (nongnu packages linux)
  #:use-module (eyrie system keys))

(define espressobin-ultra-barebones-os
  (operating-system
    (kernel linux-espressobin-ultra-5.4)
    (kernel-arguments
     (cons "console=ttyMV0,115200"
           %default-kernel-arguments))
    (initrd-modules '())
    (host-name "ebinx")
    (timezone "US/Pacific")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-bootloader)
                 (targets '("/dev/mmcblk0"))))
    (file-systems (cons
                   (file-system
                     (device "/dev/mmcblk0p1")
                     (mount-point "/")
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

(image
 (inherit espressobin-ultra-barebones-raw-image)
 (operating-system
  (operating-system
    (inherit espressobin-ultra-barebones-os)
    (packages
     (cons* rauc
            %base-packages))
    (services
     (cons* (service dhcp-client-service-type
                     (dhcp-client-configuration
                      (interfaces '("wan"))))
            (service openssh-service-type
                     (openssh-configuration
                      (openssh openssh-sans-x)
                      (permit-root-login 'prohibit-password)
                      (password-authentication? #f)
                      (authorized-keys
                       (list (list "root"
                                   %condor-ssh-key
                                   %peregrine-ssh-key)))))
            (modify-services %base-services
              (guix-service-type config =>
                                 (guix-configuration
                                  (inherit config)
                                  (authorized-keys
                                   (cons %peregrine-signing-key
                                         %default-authorized-guix-keys))))))))))

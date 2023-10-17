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
  #:use-module (nongnu packages linux)
  #:use-module (eyrie system keys))

(define linux-espressobin-ultra
  (package
    (inherit
     (customize-linux
      #:name "linux-espressobin-ultra"
      #:source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/globalscaletechnologies/linux.git")
                       (commit "linux-5.4.53-gti")))
                 (file-name (git-file-name "linux-espressobin-ultra" "5.4.53-gti"))
                 (sha256
                  (base32 "1pwpn6wrz6ydx62gp9g2drapg126lwihcr0yhhcqilc1cxy7m02q")))
      #:defconfig (local-file "gti_ccpe-88f3720_defconfig")
      #:extra-version "guix"))
    (version "5.4.53-gti")
    (home-page "https://github.com/globalscaletechnologies/linux")
    (synopsis "Linux kernel with patches from globalscale technologies")
    (description "Linux kernel build from globalscale technologies sources,
including nonfree binary blobs.")))

(define espressobin-ultra-barebones-os
  (operating-system
    (kernel linux-espressobin-ultra)
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

;; SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
;;
;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie services)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (eyrie packages)
  #:export (kitchen-configuration
            kitchen-service-type))

(define-record-type* <kitchen-configuration>
  kitchen-configuration make-kitchen-configuration
  kitchen-configuration?
  (kitchen kitchen-configuration-kitchen
           (default kitchen)))

(define %kitchen-home-directory
  "/home/kitchen")

(define %kitchen-accounts
  (list (user-group (name "kitchen") (system? #t))
        (user-account
         (name "kitchen")
         (group "kitchen")
         (system? #t)
         (comment "kitchen application user")
         (home-directory %kitchen-home-directory)
         (shell (file-append shadow "/sbin/nologin")))))

(define (kitchen-shepherd-service config)
  "Return a <shepherd-service> for kitchen CONFIG."
  (define pid-file
    (string-append %kitchen-home-directory "/kitchen.pid"))
  (define kitchen-command
    #~(list (string-append #$(kitchen-configuration-kitchen config) "/bin/kitchen")))
  (list (shepherd-service
         (provision '(kitchen))
         (documentation "Run the kitchen application server.")
         (requirement '(user-processes loopback))
         (start #~(make-forkexec-constructor
                   #$kitchen-command
                   #:user "kitchen"
                   #:group "kitchen"
                   #:log-file "/var/log/kitchen.log"
                   #:environment-variables
                   (list "RAILS_ENV=production"
                         (string-append "HOME=" #$%kitchen-home-directory)
                         (string-append "KITCHEN_PID_FILE=" #$pid-file))
                   #:pid-file #$pid-file))
         (stop #~(make-kill-destructor)))))

(define kitchen-service-type
  (service-type (name 'kitchen)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          kitchen-shepherd-service)
                       (service-extension account-service-type
                                          (const %kitchen-accounts))))
                (default-value (kitchen-configuration))
                (description "Run the kitchen web application.")))

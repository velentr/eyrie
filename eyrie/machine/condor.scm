;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie machine condor)
  #:use-module (gnu)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services base)
  #:use-module (gnu services certbot)
  #:use-module (gnu services mail)
  #:use-module (gnu services networking)
  #:use-module (gnu services security)
  #:use-module (gnu services ssh)
  #:use-module (gnu services version-control)
  #:use-module (gnu services web)
  #:use-module (eyrie system keys))

(define %condor-nftables-ruleset
  (plain-file "nftables.conf"
              "# condor firewall

table inet filter {
  chain input {
    type filter hook input priority 0; policy drop;

    # early drop of invalid connections
    ct state invalid drop

    # allow established/related connections
    ct state { established, related } accept

    # allow from loopback
    iifname lo accept

    # allow icmp
    ip protocol icmp accept
    ip6 nexthdr icmpv6 accept

    # allow ssh
    tcp dport ssh accept

    # allow http(s)
    tcp dport { http, https } accept

    # reject everything else
    reject with icmpx type port-unreachable
  }
  chain forward {
    type filter hook forward priority 0; policy drop;
  }
  chain output {
    type filter hook output priority 0; policy accept;
  }
}
"))

(define %e3r3-ssl-certificate
  "/etc/letsencrypt/live/e3r3.com/fullchain.pem")

(define %e3r3-ssl-certificate-key
  "/etc/letsencrypt/live/e3r3.com/privkey.pem")

(define %e3r3-server-configuration
  (nginx-server-configuration
   (listen '("443 ssl"))
   (ssl-certificate %e3r3-ssl-certificate)
   (ssl-certificate-key %e3r3-ssl-certificate-key)))

(define %condor-operating-system
  (operating-system
    (host-name "condor")
    (timezone "US/Pacific")
    (locale "en_US.utf8")
    (initrd-modules (append (list "virtio_scsi")
                            %base-initrd-modules))
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets '("/dev/vda"))))
    (file-systems (append
                   (list (file-system
                           (device "/dev/vda1")
                           (mount-point "/")
                           (type "ext4")))
                   %base-file-systems))
    (packages (cons* guix nss-certs %base-packages))

    (services
     (append
      (list
       (service certbot-service-type
                (certbot-configuration
                 (email "brian@kubisiak.com")
                 (certificates
                  (list (certificate-configuration
                         (domains '("e3r3.com"
                                    "cal.e3r3.com")))))))
       (service fail2ban-service-type
                (fail2ban-configuration
                 (extra-jails
                  (list
                   (fail2ban-jail-configuration
                    (name "sshd")
                    (enabled? #t))))))
       (service gitolite-service-type
                (gitolite-configuration
                 (admin-pubkey %kestrel-ssh-key)))
       (service nftables-service-type
                (nftables-configuration
                 (ruleset %condor-nftables-ruleset)))
       (service nginx-service-type
                (nginx-configuration
                 (server-blocks
                  (list
                   (nginx-server-configuration
                    (inherit %e3r3-server-configuration)
                    (server-name '("e3r3.com")))
                   (nginx-server-configuration
                    (inherit %e3r3-server-configuration)
                    (server-name '("cal.e3r3.com"))
                    (raw-content
                     '("auth_basic \"authentication required\";"
                       "auth_basic_user_file /etc/nginx/htpasswd;"))
                    (locations
                     (list
                      (nginx-location-configuration
                       (uri "/")
                       (body
                        '("proxy_pass http://localhost:5232/;"
                          "proxy_set_header X-Remote-User $remote_user;"))))))))))
       (service ntp-service-type)
       (service openssh-service-type
                (openssh-configuration
                 (openssh openssh-sans-x)
                 (permit-root-login 'prohibit-password)
                 (password-authentication? #f)
                 (authorized-keys
                  (list (list "root"
                              %condor-ssh-key
                              %kestrel-ssh-key
                              %peregrine-ssh-key)))))
       (service radicale-service-type
                (radicale-configuration
                 (config-file
                  (local-file "condor-radicale.conf"))))
       (service static-networking-service-type
                (list (static-networking
                       (addresses
                        (list (network-address
                               (device "eth0")
                               (value "146.190.112.243/20"))))
                       (routes
                        (list (network-route
                               (destination "default")
                               (gateway "146.190.112.1"))))
                       (name-servers '("84.200.69.80"
                                       "84.200.70.40"))))))
      (modify-services %base-services
        (guix-service-type config =>
                           (guix-configuration
                            (inherit config)
                            (authorized-keys
                             (cons %peregrine-signing-key
                                   %default-authorized-guix-keys)))))))))

(list
 (machine
  (operating-system %condor-operating-system)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name "e3r3.com")
    (system "x86_64-linux")
    (host-key
     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE2OqIrX1N78Rn//BEnjnWzhwrL8hqRCNUolnizb1hT0")
    (identity (string-append (getenv "HOME") "/.ssh/condor-id-ecdsa"))))))

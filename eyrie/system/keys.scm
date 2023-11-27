;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie system keys)
  #:use-module (gnu))

(define (make-public-key host type . fragments)
  (let ((name (string-append host "-" type ".pub")))
    (plain-file
     name
     (string-join
      (list type
            (apply string-append fragments)
            (string-append "bkubisiak@" host))
      " "))))

(define-public %condor-ssh-key
  (make-public-key
   "peregrine"
   "ecdsa-sha2-nistp256"
   "AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBOkg2xPVZr7hbXyEFlNc3to"
   "keZjJ9Wi9+hfsGjrvCLtiDK3TwBPh0uROwyYsrFylhijCvELz/HlYNMYFbcX/ssI="))

(define-public %kestrel-ssh-key
  (make-public-key
   "kestrel"
   "ssh-rsa"
   "AAAAB3NzaC1yc2EAAAADAQABAAABgQDJwDx7EdhjkrGQyZeeJ2QwLeF/k4RvNhIcz9vnaNNP5dH"
   "s1stTGLwuBnydGNU1BHjfDTPbjpDxcrxssvZhcPzSl59xwoBEeUdvz/Ps11cxN5X3oI68TKpX4E"
   "I8gabYtjtuBCB4cTr5LDbXiZo6+jRgm/4o0Pci8JKL0YFLohmlyiR5HZfUYIqRG2hisvWCEYBVF"
   "kQzl5Udl56HpiRW9zvQrhgDNq2JEH+AcnVPMO4/x0EWLVFOidbvO5cGGse7XUQE/U6G5weMzgC4"
   "ZtdY2QGlnVNiEDZntbG7COXQ0kzmgrypUsm2YcPmsXVqbN7srcN+9bOYCeIOvZEfoSCfPuKQEe1"
   "AIrJZbAvYfE/ZrFjS/CeXNMXkhB00LncP+P7XQcjGIWrZX79ZvW5FsW4fr8XLYACu/F+n6j67uv"
   "UM/ub+994tUzNYWiWpEIVhEXkYulwz/nTWjadVLJ+KSOSwYwA7NoSxdXyOPPBVUy94K1ZdPKUqT"
   "nrNyvIxLhz9J2J+pRE="))

(define-public %peregrine-ssh-key
  (make-public-key
   "peregrine"
   "ssh-rsa"
   "AAAAB3NzaC1yc2EAAAADAQABAAACAQDYo9T9czEM9aS/N6LDkgioutTDnQqtRrNocc7EJiN/eoD"
   "Z1tGrWt02rLdlx3jYDXsi0RqIhtpQdnJ/TmF+AbVlzvGeqYom1HMQk6qFAh2W7xk5AKUDs35wx9"
   "T+UTYIVeNXEtdkF1NY1gZA6Sw3Bb0zXdB3w32+sHUlFdFYqOAy5lGgVIzoC/qboGF2dkcDHo1qW"
   "yacMsRX1iFMxHRxntyUz/Xke9UXJ+Snx/FwWP5Unucd9eq7pS9h30+KLutAzRaidgSpJGh96RiX"
   "eIvYIAeJFcq0bUrdMZm31Tjw+Bv/d7ef76KRLhsHrknrFQXqv7jU7VhwsqkCkl7640XX0Hirtgk"
   "4J1pHM8sae63oVmPowCpMzqAF4GSgocAbcPPz5FvZlhw7f6gY7H/h99W03CbJ4JvlLRdZul42fC"
   "xdojNbE5n8uu6kKOGVJgmKI9og8mxj3jHGRohaSI1S2xJ9ENahFGAxKRQEJ3rN0lkAi/8q5PoAq"
   "Sf2dNue0BJAMPdALiFSyyj9qx4IB+KYBvSyoAH0XjSAVC+To0DVJV6PpETJqf1TjxL2h8MrUDcg"
   "lNF0BtS+n4dyt49owTAIGx1L3TqTaDD+bIinx4ppZvTokdrikzvrgeB5ziNzTwF/OfWKcFHeLqk"
   "toSyoBG+h9xYhypXyO4EBB2V7UprCmqrnM0xYYw=="))

(define-public %peregrine-signing-key
  (local-file "peregrine-signing-key.pub"))

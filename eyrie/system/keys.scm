;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie system keys)
  #:use-module (gnu))

(define-public %condor-ssh-key
  (local-file "condor-id-ecdsa.pub"))

(define-public %kestrel-ssh-key
  (local-file "kestrel-id-rsa.pub"))

(define-public %peregrine-ssh-key
  (local-file "peregrine-id-rsa.pub"))

(define-public %peregrine-signing-key
  (local-file "peregrine-signing-key.pub"))

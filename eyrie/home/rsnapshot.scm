;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (eyrie home rsnapshot)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-rsnapshot-service-type
            home-rsnapshot-configuration))

(define-configuration/no-serialization home-rsnapshot-configuration
  (rsnapshot (file-like rsnapshot) "The rsnapshot package to use.")
  (rsync (file-like rsync) "The rsync package to use.")
  (ssh (file-like openssh-sans-x) "The SSH package to use.")
  (directory (string (string-append (getenv "HOME") "/bak"))
             "Local directory to store backed-up files."))

(define (rsnapshot-configuration config)
  (match-record config <home-rsnapshot-configuration>
                (directory rsnapshot rsync ssh)
    (let ((cmds
           (cons*
            "cmd_cp\t" coreutils "/bin/cp\n"
            "cmd_rm\t" coreutils "/bin/rm\n"
            "cmd_rsync\t" rsync "/bin/rsync\n"
            "cmd_du\t" coreutils "/bin/du\n"
            "cmd_rsnapshot_diff\t" rsnapshot "/bin/rsnapshot-diff\n"
            (if ssh (list "cmd_ssh\t" ssh "/bin/ssh\n") '()))))
      (apply
       mixed-text-file
       (append
        (list
         "rsnapshot.conf"
         "config_version\t1.2\n"
         "snapshot_root\t" directory "\n"
         "verbose\t2\n"
         "loglevel\t3\n"
         "lockfile\t/run/user/" (number->string (getuid)) "/rsnapshot.pid\n")
        cmds

        (list
         "retain\talpha\t7\n"
         "backup\troot@e3r3.com:/var/lib/radicale/\te3r3/\n"))))))

(define (home-rsnapshot-configuration-jobs config)
  (match-record config <home-rsnapshot-configuration>
                (rsnapshot)
    (list #~(job '(next-hour '(0))  ; daily at midnight
                 (string-append #$rsnapshot "/bin/rsnapshot -c "
                                #$(rsnapshot-configuration config)
                                " alpha")))))

(define home-rsnapshot-service-type
  (service-type
   (name 'home-rsnapshot)
   (extensions
    (list (service-extension home-mcron-service-type
                             home-rsnapshot-configuration-jobs)))
   (default-value (home-rsnapshot-configuration))
   (description
    "Configure backups using rsnapshot.")))

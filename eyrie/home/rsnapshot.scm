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
  #:use-module (srfi srfi-26)
  #:export (home-rsnapshot-service-type
            home-rsnapshot-configuration))

(define (list-or-string? val)
  (or (list? val)
      (string? val)))

(define-configuration/no-serialization rsnapshot-backup-level
  (name string "Name of the backup level.")
  (retention integer "Number of backups to keep for this interval.")
  (interval list-or-string "An mcron time specification indicating how often to
back up this level."))

(define (rsnapshot-backup-level->retain-configuration level)
  (match-record level <rsnapshot-backup-level>
                (name retention)
    (format #f "retain\t~a\t~d\n" name retention)))

(define (rsnapshot-backup-level->mcron-job level make-rsnapshot-command)
  (match-record level <rsnapshot-backup-level>
                (name interval)
    #~(job #$interval
           #$(make-rsnapshot-command name))))

(define-configuration/no-serialization home-rsnapshot-configuration
  (rsnapshot (file-like rsnapshot) "The rsnapshot package to use.")
  (rsync (file-like rsync) "The rsync package to use.")
  (ssh (file-like openssh-sans-x) "The SSH package to use.")
  (directory (string (string-append (getenv "HOME") "/bak"))
             "Local directory to store backed-up files.")
  (backups (list '()) "List of backups to make.")
  (backup-levels (list
                  (list
                   (rsnapshot-backup-level
                    (name "alpha")
                    (retention 6)
                    (interval ''(next-hour '(0))))  ; daily at midnight
                   (rsnapshot-backup-level
                    (name "beta")
                    (retention 4)
                    (interval "15 0 * * 1"))  ; Monday 12:15 AM
                   (rsnapshot-backup-level
                    (name "gamma")
                    (retention 6)
                    ;; 12:30 AM on the first of every month
                    (interval ''(next-minute-from (next-day '(1)) '(30))))))
                 "List of backup levels to keep."))

(define (rsnapshot-configuration config)
  (define (make-backup-settings backups)
    (apply
     append
     (map (lambda (backup)
            (list "backup\t" (car backup) "\t" (cadr backup) "\n"))
          backups)))
  (match-record config <home-rsnapshot-configuration>
                (backups backup-levels directory rsnapshot rsync ssh)
    (let ((cmds
           (cons*
            "cmd_cp\t" coreutils "/bin/cp\n"
            "cmd_rm\t" coreutils "/bin/rm\n"
            "cmd_rsync\t" rsync "/bin/rsync\n"
            "cmd_du\t" coreutils "/bin/du\n"
            "cmd_rsnapshot_diff\t" rsnapshot "/bin/rsnapshot-diff\n"
            (if ssh (list "cmd_ssh\t" ssh "/bin/ssh\n") '())))
          (backups (make-backup-settings backups)))
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

        (map rsnapshot-backup-level->retain-configuration backup-levels)
        backups)))))

(define (home-rsnapshot-configuration-jobs config)
  (match-record config <home-rsnapshot-configuration>
                (rsnapshot backup-levels)
    (define (make-rsnapshot-command level-name)
      #~(string-append #$rsnapshot "/bin/rsnapshot -c "
                       #$(rsnapshot-configuration config)
                       " " #$level-name))
    (map (cut rsnapshot-backup-level->mcron-job <> make-rsnapshot-command)
         backup-levels)))

(define home-rsnapshot-service-type
  (service-type
   (name 'home-rsnapshot)
   (extensions
    (list (service-extension home-mcron-service-type
                             home-rsnapshot-configuration-jobs)))
   (default-value (home-rsnapshot-configuration))
   (description
    "Configure backups using rsnapshot.")))
